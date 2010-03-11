# -*- coding: utf-8 -*-
#
# This file is part of couchapp released under the Apache 2 license. 
# See the NOTICE for more information.

from __future__ import with_statement
import base64
import itertools
import mimetypes
import re
import tempfile

from couchapp import __version__
from couchapp.errors import ResourceNotFound, ResourceConflict,\
PreconditionFailed, RequestFailed, BulkSaveError
from couchapp.restkit import Resource, HttpResponse, ResourceError, request
from couchapp.restkit import util
import couchapp.simplejson as json

USER_AGENT = "couchapp/%s" % __version__

aliases = {
    'id': '_id',
    'rev': '_rev' 
}

class CouchDBResponse(HttpResponse):
    
    @property
    def json_body(self):
        try:
            return json.load(self.body_file)
        except ValueError:
            return self.body
   
def couchdb_version(server_uri):
    resp = request(server_uri, headers=[("Accept", "application/json")])
    version = json.load(resp.body_file)["version"]
    t = []
    for p in version.split("."):
        try:
            t.append(int(p))
        except ValueError:
            continue
    
    return tuple(t)   

class Uuids(Resource):
    
    def __init__(self, uri, max_uuids=1000):
        Resource.__init__(self, uri=uri, response_class=CouchDBResponse)
        self._uuids = []
        self.max_uuids = max_uuids
        
    def next(self):
        if not self._uuids:
            self.fetch_uuids()
        self._uuids, res = self._uuids[:-1], self._uuids[-1]
        return res
        
    def __iter__(self):
        return self
        
    def fetch_uuids(self):
        count = self.max_uuids - len(self._uuids)
        resp = self.get('/_uuids', count=count)
        self._uuids += resp.json_body['uuids']

class Database(Resource):
    
    def __init__(self, ui, uri, create=True):
        Resource.__init__(self, uri=uri, response_class=CouchDBResponse)

        self.ui = ui
        self.server_uri = uri.rsplit('/', 1)[0]
        self.uuids = Uuids(self.server_uri)
        self.version = couchdb_version(self.server_uri)
        
        # create the db
        try:
            self.head()
        except ResourceNotFound:
            if not create:
                raise
            self.put()
        
    def info(self):
        return self.get().json_body
        
    def all_docs(self, **params):
        res = self.get('_all_docs', **params)
        return res.json_body
            
    def open_doc(self, docid=None, wrapper=None, *params):
        docid = docid or self.uuids.next()
        resp = self.get(escape_docid(docid), *params)
        
        if wrapper is not None:
            if not callable(wrapper):
                raise TypeError("wrapper isn't a callable")
            return wrapper(resp)
        return resp.json_body
        
    def save_doc(self, doc=None, encode=False, force=False, **params):
        doc = doc or {}
        if '_attachments' in doc and (encode and self.version < (0, 11)):
            doc['_attachments'] = encode_attachments(doc['_attachments'])
         
        with tempfile.TemporaryFile() as json_stream:
            # we use a temporary file to send docs to reduce 
            # memory usage
            json.dump(doc, json_stream)
            
            res = None
            if '_id' in doc:
                docid = escape_docid(doc['_id'])
                try:
                    res = self.put(docid, payload=json_stream, **params)
                except ResourceConflict:
                    if force:
                        rev = self.last_rev(doc['_id'])
                        doc['_rev'] = rev
                        res = self.put(docid, payload=json_stream, **params)
                    else:
                        raise
            else:
                try:
                    doc['_id'] = self.uuids.next()
                    res = self.put(doc['_id'], payload=json_stream, **params)
                except ResourceConflict:
                    res = self.post(payload=json_stream, **params)
                
            json_res = res.json_body
            doc1 = doc
            for a, n in aliases.items():
                if a in json_res:
                    doc1[n] = json_res[a]
                    
            return doc1
   
    def last_rev(self, docid):
        r = self.head(escape_docid(docid))
        return r.headers['etag'].strip('"')
        
    def delete_doc(self, id_or_doc):
        if isinstance(id_or_doc, types.StringType):
            docid = id_or_doc
            self.delete(escape_docid(id_or_doc), rev=self.last_rev(id_or_doc))
            
        else:
            docid = id_or_doc.get('_id')
            if not docid:
                raise ValueError('Not valid doc to delete (no doc id)')
            rev = id_or_doc.get('_rev', self.last_rev(docid))
            self.delete(escape_docid(docid), rev=rev)
            
    def save_docs(self, docs, all_or_nothing=False, use_uuids=True):       
        def is_id(doc):
            return '_id' in doc
            
        if use_uuids:
            ids = []
            noids = []
            for k, g in itertools.groupby(docs, is_id):
                if not k:
                    noids = list(g)
                else:
                    ids = list(g)
                    
            for doc in noids:
                nextid = self.uuids.next()
                if nextid:
                    doc['_id'] = nextid
                    
        payload = { "docs": docs }
        if all_or_nothing:
            payload["all-or-nothing"] = True
            
        # update docs
        with tempfile.TemporaryFile() as json_stream:
            json.dump(payload, json_stream)
            res = self.post('/_bulk_docs', payload=json_stream)
            
        json_res = res.json_body
        errors = []
        docs1 = docs
        for i, r in enumerate(json_res):
            if 'error' in r:
                errors.append(s)
            else:
                docs1[i].update({'_id': r['id'], 
                                '_rev': r['rev']})
        if errors:
            raise BulkSaveError(docs1, errors)
        return docs1
            
    def delete_docs(self, docs, all_or_nothing=False):
        for doc in docs:
            doc['_deleted'] = True
        return self.save_docs(docs, all_or_nothing=all_or_nothing)

    def fetch_attachment(self, id_or_doc, name):
        if isinstance(id_or_doc, basestring):
            docid = id_or_doc
        else:
            docid = id_or_doc['_id']
      
        return self.get("%s/%s" % (escape_docid(docid), name))
        
    def put_attachment(self, doc, content=None, name=None, content_type=None, 
            content_length=None):
            
        headers = {}
        
        if not content:
            content = ""
            content_length = 0
            
        if name is None:
            if hasattr(content, "name"):
                name = content.name
            else:
                raise InvalidAttachment(
                            'You should provid a valid attachment name')
        name = util.url_quote(name, safe="")
        
        if content_type is None:
            content_type = ';'.join(filter(None, mimetypes.guess_type(name)))

        if content_type:
            headers['Content-Type'] = content_type
            
        # add appropriate headers    
        if content_length and content_length is not None:
            headers['Content-Length'] = content_length

        res = self.put("%s/%s" % (escape_docid(doc['_id']), name), 
                    payload=content, headers=headers, rev=doc['_rev'])
        json_res = res.json_body
        
        if 'ok' in json_res:
            return self.open_doc(doc['_id'])
        return False
        
                    
    def delete_attachment(self, doc, name):
        name = resource.url_quote(name, safe="")
        json_res = self.delete("%s/%s" % (escape_docid(doc['_id']), name), 
                        rev=doc['_rev']).json_body
        return self.open_doc(doc['_id'])
         
    def request(self, *args, **params):
        headers = params.get('headers') or {}
        headers.setdefault('Accept', 'application/json')
        headers.setdefault('User-Agent', USER_AGENT)
        params['headers'] = headers
        try:
            return Resource.request(self, *args, **params)
        except  ResourceError, e:
            msg = getattr(e, 'msg', '')
            if e.response and msg:
                if e.response.headers.get('content-type') == 'application/json':
                    try:
                        msg = json.loads(msg)
                    except ValueError:
                        pass
                    
            if type(msg) is dict:
                error = msg.get('reason')
            else:
                error = msg
                
            if e.status_int == 404:
                raise ResourceNotFound(error, http_code=404,
                        response=e.response)

            elif e.status_int == 409:
                raise ResourceConflict(error, http_code=409,
                        response=e.response)
            elif e.status_int == 412:
                raise PreconditionFailed(error, http_code=412,
                        response=e.response)
                        
            elif e.status_int in (401, 403):
                raise Unauthorized(str(error))
            else:
                RequestFailed(str(e))
        except Exception, e:
            raise RequestFailed(str(e))
            
    def __getitem__(self, docid):
        return self.open_doc(docid)
        
    def __delitem__(self, docid):
        return self.delete_doc(docid)

    def __setitem__(self, docid, doc):
        doc['_id'] = doc
        self.save_doc(doc)
        
    def __contains__(self, docid):
        self.head(escape_docid(docid))
        
    def __len__(self):
        return self.info()['doc_count']
        
    def __nonzero__(self):
        return (len(self) > 0)
                

def encode_params(params):
    """ encode parameters in json if needed """
    _params = {}
    if params:
        for name, value in params.items():
            if value is None:
                continue
            if name in ('key', 'startkey', 'endkey') \
                    or not isinstance(value, basestring):
                value = json.dumps(value).encode('utf-8')
            _params[name] = value
    return _params    
    
def escape_docid(docid):
    if docid.startswith('/'):
        docid = docid[1:]
    if docid.startswith('_design'):
        docid = '_design/%s' % util.url_quote(docid[8:], safe='')
    else:
        docid = util.url_quote(docid, safe='')
    return docid
    
def encode_attachments(attachments):
    for k, v in attachments.iteritems():
        if v.get('stub', False):
            continue
        else:
            re_sp = re.compile('\s')
            v['data'] = re_sp.sub('', base64.b64encode(v['data']))
    return attachments
