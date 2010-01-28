# -*- coding: utf-8 -*-
#
# Copyright 2008,2009 Benoit Chesneau <benoitc@e-engura.org>
#
#  Licensed under the Apache License, Version 2.0 (the "License");
#  you may not use this file except in compliance with the License.
#  You may obtain a copy of the License at#
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
#  Unless required by applicable law or agreed to in writing, software
#  distributed under the License is distributed on an "AS IS" BASIS,
#  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#  See the License for the specific language governing permissions and
#  limitations under the License.

from itertools import groupby
import mimetypes
import urllib

try:
    import json
except ImportError:
    import couchapp.simplejson as json 

from couchapp.errors import *
import couchapp.couchdbresource as resource

class Server(object):
    """ Server object that allows you to access and manage a couchdb node. 
    A Server object can be used like any `dict` object.
    """
    
    def __init__(self, ui, url='http://127.0.0.1:5984', 
            uuid_batch_count=resource.DEFAULT_UUID_BATCH_COUNT, 
            timeout=resource.DEFAULT_TIMEOUT, max_connections=resource.MAX_CONNECTIONS, 
            key_file=None, cert_file=None):
        """ constructor for Server object
        
        @param uri: uri of CouchDb host
        @param uuid_batch_count: max of uuids to get in one time
        @param transport: an transport instance from :mod:`restkit.transport`. Can be used
                to manage authentification to your server or proxy.
        @param use_proxy: boolean, default is False, if you want to use a proxy
        @param timeout: connection timeout, delay after a connection should be released
        @param max_size: maximum number of connection in the pool
        @param cert_file: cert file for ssl connections
        @param key_file: key file for ssl connections
        
        """
        
        if not url or url is None:
            raise ValueError("Server url is missing")

        self.ui = ui
        self.url = url
        self.timeout = timeout
        self.max_connections = max_connections
        self.key_file = key_file
        self.cert_file = cert_file
        self.uuid_batch_count = uuid_batch_count
        self._uuid_batch_count = uuid_batch_count
        
        self.res = resource.CouchDBResource(ui, url, timeout=timeout, 
                            max_connections=max_connections, 
                            key_file=key_file, cert_file=cert_file)
        self.uuids = []
        
    def info(self, _raw_json=False):
        """ info of server 
        @param _raw_json: return raw json instead deserializing it
        
        @return: dict
        
        """
        return self.res.get(_raw_json=_raw_json)
    
    def all_dbs(self, _raw_json=False):
        """ get list of databases in CouchDb host 
        
        @param _raw_json: return raw json instead deserializing it
        """
        result = self.res.get('/_all_dbs', _raw_json=_raw_json)
        return result
        
    def create_db(self, dbname):
        """ Create a database on CouchDb host

        @param dname: str, name of db

        @return: Database instance if it's ok or dict message
        """
        _dbname = resource.url_quote(dbname, safe="")
        res = self.res.put('/%s/' % _dbname)
        if res['ok']:
            dburl = resource.make_uri(self.url, "/%s" % _dbname) 
            return Database(self.ui, dburl, server=self)
        return res['ok']

    def get_or_create_db(self, dbname):
        """
        Try to return a Database object for dbname. If 
        database doest't exist, it will be created.
        
        """
        try:
            return self[dbname]
        except ResourceNotFound:
            return self.create_db(dbname)
        
    def delete_db(self, dbname):
        """
        Delete database
        """
        del self[dbname]
        
    #TODO: maintain list of replications
    def replicate(self, source, target, continuous=False):
        """
        simple handler for replication
        
        @param source: str, URI or dbname of the source
        @param target: str, URI or dbname of the target
        @param continuous: boolean, default is False, set the type of replication
        
        More info about replication here : 
        http://wiki.apache.org/couchdb/Replication
        
        """
        self.res.post('/_replicate', payload={
            "source": source,
            "target": target,
            "continuous": continuous
        })
        
    def next_uuid(self, count=None):
        """
        return an available uuid from couchapp
        """
        if count is not None:
            self._uuid_batch_count = count
        else:
            self._uuid_batch_count = self.uuid_batch_count
        
        self.uuids = self.uuids or []
        if not self.uuids:
            self.uuids = self.res.get('/_uuids', count=self._uuid_batch_count)["uuids"]
        return self.uuids.pop()
        
    def __getitem__(self, dbname):
        if dbname in self:
            dburl = resource.make_uri(self.url, "/%s" % dbname) 
            return Database(self.ui, dburl, server=self)
        raise ResourceNotFound("database %s not found" % dbname)
        
    def __delitem__(self, dbname):
        return self.res.delete('/%s/' % resource.url_quote(dbname, safe=":"))
        
    def __contains__(self, dbname):
        if dbname in self.all_dbs():
            return True
        return False
        
    def __iter__(self):
        for dbname in self.all_dbs():
            dburl = resource.make_uri(self.url, "/%s" % dbname) 
            yield Database(self.ui, dburl, server=self)

    def __len__(self):
        return len(self.all_dbs())
        
    def __nonzero__(self):
        return (len(self) > 0)

class Database(object):
    """ Object that abstract access to a CouchDB database
    A Database object can act as a Dict object.
    """

    def __init__(self, ui, dburl, server=None, create=False):
        """Constructor for Database

        @param dburl: string, full encoded dburl
        @param server: Server instance
        """

        if server is not None and not hasattr(server, 'next_uuid'):
            raise TypeError('%s is not a couchapp.http.Server instance' % 
                            server.__class__.__name__)
        if dburl.endswith("/"):
            dburl = dburl[:-1]
                     
        self.url = dburl
        server_url, dbname = dburl.rsplit("/", 1)
        self.dbname = urllib.unquote(dbname)
        self.ui = ui
        if server is None:
            server = Server(ui, server_url)
            
        if create and not self.dbname in server.all_dbs():
            try:
                res = server.res.put('/%s/' % dbname)
            except PreconditionFailed:
                if self.ui.verbose >= 1:
                    self.ui.logger.info("database %s not created, already exists" % dbname)
            if ui.verbose >= 1 and res['ok']:
                self.ui.logger.info("database %s created." % dbname)
                        
        self.server = server
        self.res = server.res.clone()
        if "/" in dbname:
            self.res.client.safe = ":/%"
        self.res.update_uri('/%s' % resource.url_quote(dbname, safe="%"))
    
    def __repr__(self):
        return "<%s %s>" % (self.__class__.__name__, self.dbname)
        
        
    def info(self, _raw_json=False):
        """
        Get database information
        
        @param _raw_json: return raw json instead deserializing it
        
        @return: dict
        """
        data = self.res.get(_raw_json=_raw_json)
        return data
        
    def compact(self, dname=None, _raw_json=False):
        """ compact database
        @param dname: string, name of design doc. Usefull to 
        compact a view.
        """
        path = "/_compact"
        if dname is not None:
            path = "%s/%s" % (path, resource.escape_docid(dname))
        res = self.res.post(path, _raw_json=_raw_json)
        return res
        
    def view_cleanup(self, _raw_json=False):
        res = self.res.post('/_view_cleanup, _raw_json')
        return res

    def flush(self):
        _design_docs = [self[i["id"]] for i in self.all_docs(startkey="_design", 
                                                        endkey="_design/"+u"\u9999")]
        self.server.delete_db(self.dbname)
        self.server.create_db(self.dbname)
        [i.pop("_rev") for i in _design_docs]
        self.save_docs( _design_docs)
        
    def doc_exist(self, docid):
        """Test if document exists in a database

        @param docid: str, document id
        @return: boolean, True if document exist
        """

        try:
            data = self.res.head(resource.escape_docid(docid))
        except ResourceNotFound:
            return False
        return True
        
    def get_doc(self, docid, wrapper=None, **params):
        """Get document from database
        
        Args:
        @param docid: str, document id to retrieve 
        @param rev: if specified, allows you to retrieve
        a specific revision of document
        @param wrapper: callable. function that takes dict as a param. 
        Used to wrap an object.
        @param _raw_json: return raw json instead deserializing it
        
        @return: dict, representation of CouchDB document as
         a dict.
        """
        docid = resource.escape_docid(docid)
        doc = self.res.get(docid, **params)

        if wrapper is not None:
            if not callable(wrapper):
                raise TypeError("wrapper isn't a callable")
            return wrapper(doc)
        
        return doc

    def all_docs(self, **params):
        """Get all documents from a database

        This method has the same behavior as a view.

        `all_docs( **params )` is the same as `view('_all_docs', **params)`
         and `all_docs( by_seq=True, **params)` is the same as
        `view('_all_docs_by_seq', **params)`

        You can use all(), one(), first() just like views

        Args:
        @param by_seq: bool, if True the "_all_docs_by_seq" is passed to
        couchdb. It will return an updated list of all documents.

        @return: list, results of the view
        """
        return self.view('_all_docs', **params)
        
    def doc_revisions(self, docid, with_doc=True, _raw_json=False):
        """ retrieve revisions of a doc
            
        @param docid: str, id of document
        @param with_doc: bool, if True return document
        dict with revisions as member, if false return 
        only revisions
        @param _raw_json: return raw json instead deserializing it
        
        @return: dict: '_rev_infos' member if you have set with_doc
        to True :
        

                {
                    "_revs_info": [
                        {"rev": "123456", "status": "disk"},
                            {"rev": "234567", "status": "missing"},
                            {"rev": "345678", "status": "deleted"},
                    ]
                }
            
        If False, return current revision of the document, but with
        an additional field, _revs, the value being a list of 
        the available revision IDs. 
        """
        docid = resource.escape_docid(docid)
        try:
            if with_doc:
                doc_with_rev = self.res.get(docid, _raw_json=_raw_json, revs=True)
            else:
                doc_with_revs = self.res.get(docid, _raw_json=_raw_json, revs_info=True)
        except ResourceNotFound:
            if self.ui.verbose >=2:
                self.ui.logger.info("document %s not found" % docid)
            return None
        return doc_with_revs           
        
    def save_doc(self, doc, encode_attachments=False, **params):
        """ Save a document. It will use the `_id` member of the document 
        or request a new uuid from CouchDB. IDs are attached to
        documents on the client side because POST has the curious property of
        being automatically retried by proxies in the event of network
        segmentation and lost responses. (Idee from `Couchrest <http://github.com/jchris/couchrest/>`)

        @param doc: dict.  doc is updated 
        with doc '_id' and '_rev' properties returned 
        by CouchDB server when you save.
        @param _raw_json: return raw json instead deserializing it
        @param params, list of optionnal params, like batch="ok"
        
        with `_raw_json=True` It return raw response. If False it update 
        doc instance with new revision (if batch=False).
        """
        if doc is None:
            doc = {}
            
        _raw_json = params.get('_raw_json', False)
            
        if '_attachments' in doc and encode_attachments:
            doc['_attachments'] = resource.encode_attachments(doc['_attachments'])
            
        if '_id' in doc:
            docid = resource.escape_docid(doc['_id'])
            res = self.res.put(docid, payload=doc, _raw_json=_raw_json, **params)
        else:
            try:
                doc['_id'] = self.server.next_uuid()
                res = self.res.put(doc['_id'], payload=doc, **params)
            except:
                res = self.res.post(payload=doc, **params)
                
        if _raw_json:    
            return res
                
        if 'batch' in params and 'id' in res:
            doc.update({ '_id': res['id']})
        else:
            doc.update({ '_id': res['id'], '_rev': res['rev']})
        
    def save_docs(self, docs, use_uuids=True, all_or_nothing=False, _raw_json=False):
        """ bulk save. Modify Multiple Documents With a Single Request
        
        @param docs: list of docs
        @param use_uuids: add _id in doc who don't have it already set.
        @param all_or_nothing: In the case of a power failure, when the database 
        restarts either all the changes will have been saved or none of them. 
        However, it does not do conflict checking, so the documents will
        @param _raw_json: return raw json instead deserializing it
        be committed even if this creates conflicts.
        
        With `_raw_json=True` it return raw response. When False it return anything
        but update list of docs with new revisions and members (like deleted)
        
        .. seealso:: `HTTP Bulk Document API <http://wiki.apache.org/couchdb/HTTP_Bulk_Document_API>`
        
        """
        # we definitely need a list here, not any iterable, or groupby will fail
        docs = list(docs)
                
        def is_id(doc):
            return '_id' in doc
            
        if use_uuids:
            ids = []
            noids = []
            for k, g in groupby(docs, is_id):
                if not k:
                    noids = list(g)
                else:
                    ids = list(g)
            
            uuid_count = max(len(noids), self.server.uuid_batch_count)
            for doc in noids:
                nextid = self.server.next_uuid(count=uuid_count)
                if nextid:
                    doc['_id'] = nextid
                    
        payload = { "docs": docs }
        if all_or_nothing:
            payload["all-or-nothing"] = True
            
        # update docs
        results = self.res.post('/_bulk_docs', payload=payload, _raw_json=_raw_json)
        
        if _raw_json:
            return results
        
        errors = []
        for i, res in enumerate(results):
            if 'error' in res:
                errors.append(res)
            else: 
                docs[i].update({'_id': res['id'], '_rev': res['rev']})
        if errors:
            raise BulkSaveError(errors)
    
    
    def delete_docs(self, docs, all_or_nothing=False, _raw_json=False):
        """ bulk delete. 
        It adds '_deleted' member to doc then uses bulk_save to save them.
        
        @param _raw_json: return raw json instead deserializing it
        
        With `_raw_json=True` it return raw response. When False it return anything
        but update list of docs with new revisions and members.
        
        """
        for doc in docs:
            doc['_deleted'] = True
        self.save_docs(docs, use_uuids=False, all_or_nothing=all_or_nothing,
                    _raw_json=_raw_json)
 
    def delete_doc(self, doc, _raw_json=False):
        """ delete a document or a list of documents
        @param doc: str or dict,  document id or full doc.
        @param _raw_json: return raw json instead deserializing it
        @return: dict like:
       
        .. code-block:: python

            {"ok":true,"rev":"2839830636"}
        """
        result = { 'ok': False }
        if isinstance(doc, dict):
            if not '_id' or not '_rev' in doc:
                raise KeyError('_id and _rev are required to delete a doc')
                
            docid = resource.escape_docid(doc['_id'])
            result = self.res.delete(docid, _raw_json=_raw_json, rev=doc['_rev'])
        elif isinstance(doc, basestring): # we get a docid
            docid = resource.escape_docid(doc)
            data = self.res.head(docid)
            response = self.res.response
            result = self.res.delete(docid, raw_json=_raw_json, 
                                    rev=response['etag'].strip('"'))
        return result
        
    def copy_doc(self, doc, dest=None, _raw_json=False):
        """ copy an existing document to a new id. If dest is None, a new uuid will be requested
        @param doc: dict or string, document or document id
        @param dest: basestring or dict. if _rev is specified in dict it will override the doc
        @param _raw_json: return raw json instead deserializing it
        """
        if isinstance(doc, basestring):
            docid = doc
        else:
            if not '_id' in doc:
                raise KeyError('_id is required to copy a doc')
            docid = doc['_id']
        
        if dest is None:
            destination = self.server.next_uuid(count=1)   
        elif isinstance(dest, basestring):
            if dest in self:
                dest = self.get_doc(dest)['_rev']
                destination = "%s?rev=%s" % (dest['_id'], dest['_rev'])
            else:
                destination = dest
        elif isinstance(dest, dict):
            if '_id' in dest and '_rev' in dest and dest['_id'] in self:
                rev = dest['_rev']
                destination = "%s?rev=%s" % (dest['_id'], dest['_rev'])
            else:
                raise KeyError("dest doesn't exist or this not a document ('_id' or '_rev' missig).")
    
        if destination:
            result = self.res.copy('/%s' % docid, headers={ "Destination": str(destination) },
                                _raw_json=_raw_json)
            return result
            
        result = { 'ok': False }
        if raw_json:
            return json.dumps(result).encode('utf-8')
        return result
            
        
    def view(self, view_name, wrapper=None, **params):
        """ get view results from database. viewname is generally 
        a string like `designname/viewnam". It return an ViewResults
        object on which you could iterate, list, ... . You could wrap
        results in wrapper function, a wrapper function take a row
        as argument. Wrapping could be also done by passing an Object 
        in obj arguments. This Object should have a `wrap` method
        that work like a simple wrapper function.
        
        @param view_name, string could be '_all_docs', '_all_docs_by_seq',
        'designname/viewname' if view_name start with a "/" it won't be parsed
        and beginning slash will be removed. Usefull with c-l for example.
        @param wrapper: function used to wrap results 
        @param params: params of the view
        
        """
        if view_name.startswith('/'):
            view_name = view_name[1:]
        if view_name == '_all_docs':
            view_path = view_name
        elif view_name == '_all_docs_by_seq':
            view_path = view_name
        else:
            view_name = view_name.split('/')
            dname = view_name.pop(0)
            vname = '/'.join(view_name)
            view_path = '_design/%s/_view/%s' % (dname, vname)

        return View(self, view_path, wrapper=wrapper)(**params)

    def temp_view(self, design, wrapper=None, **params):
        """ get adhoc view results. Like view it reeturn a ViewResult object."""
        return TempView(self, design, wrapper=wrapper)(**params)
        
    def search( self, view_name, handler='_fti', wrapper=None, **params):
        """ Search. Return results from search. Use couchdb-lucene 
        with its default settings by default."""
        return View(self, "/%s/%s" % (handler, view_name), wrapper=wrapper)(**params)

    def documents(self, wrapper=None, **params):
        """ return a ViewResults objects containing all documents. 
        This is a shorthand to view function.
        """
        return View(self, '_all_docs', wrapper=wrapper)(**params)
    iterdocuments = documents

    def put_attachment(self, doc, content, name=None, content_type=None, 
        content_length=None):
        """ Add attachement to a document. All attachments are streamed.

        @param doc: dict, document object
        @param content: string or :obj:`File` object.
        @param name: name or attachment (file name).
        @param content_type: string, mimetype of attachment.
        If you don't set it, it will be autodetected.
        @param content_lenght: int, size of attachment.

        @return: bool, True if everything was ok.


        Example:
            
            >>> from simplecouchdb import server
            >>> server = server()
            >>> db = server.create_db('couchapp_test')
            >>> doc = { 'string': 'test', 'number': 4 }
            >>> db.save(doc)
            >>> text_attachment = u'un texte attaché'
            >>> db.put_attachment(doc, text_attachment, "test", "text/plain")
            True
            >>> file = db.fetch_attachment(doc, 'test')
            >>> result = db.delete_attachment(doc, 'test')
            >>> result['ok']
            True
            >>> db.fetch_attachment(doc, 'test')
            >>> del server['couchapp_test']
            {u'ok': True}
        """
        headers = {}
        
        if not content:
            content = ""
            content_length = 0
        if name is None:
            if hasattr(content, "name"):
                name = content.name
            else:
                raise InvalidAttachment('You should provid a valid attachment name')
        name = resource.url_quote(name, safe="")
        if content_type is None:
            content_type = ';'.join(filter(None, mimetypes.guess_type(name)))

        if content_type:
            headers['Content-Type'] = content_type
            
        # add appropriate headers    
        if content_length and content_length is not None:
            headers['Content-Length'] = content_length

        result = self.res(resource.escape_docid(doc['_id'])).put(name, payload=content, 
                headers=headers, rev=doc['_rev'])

        if result['ok']:
            doc.update({ '_rev': result['rev']})
        return result['ok']

    def delete_attachment(self, doc, name):
        """ delete attachement to the document

        @param doc: dict, document object in python
        @param name: name of attachement
    
        @return: dict, with member ok set to True if delete was ok.
        """
        name = resource.url_quote(name, safe="")
        
        res = self.res(resource.escape_docid(doc['_id'])).delete(name, rev=doc['_rev'])
        if res['ok']:
            doc.update({ '_rev': res['rev']})
        return res['ok']
        

    def fetch_attachment(self, id_or_doc, name, stream=False):
        """ get attachment in a document
        
        @param id_or_doc: str or dict, doc id or document dict
        @param name: name of attachment default: default result
        @param stream: boolean, response return a ResponseStream object
        @param stream_size: int, size in bytes of response stream block
        
        @return: str, attachment
        """

        if isinstance(id_or_doc, basestring):
            docid = id_or_doc
        else:
            docid = id_or_doc['_id']
      
        name = resource.url_quote(name, safe="")
        return self.res(resource.escape_docid(docid)).get(name, _stream=stream)

    def ensure_full_commit(self, _raw_json=False):
        """ commit all docs in memory """
        return self.res.post('_ensure_full_commit', _raw_json=_raw_json)
        
    def __len__(self):
        return self.info()['doc_count'] 
        
    def __contains__(self, docid):
        return self.doc_exist(docid)
        
    def __getitem__(self, docid):
        return self.get_doc(docid)
        
    def __setitem__(self, docid, doc):
        doc['_id'] = docid
        self.save_doc(doc)
        
        
    def __delitem__(self, docid):
       self.delete_doc(docid)

    def __iter__(self):
        return self.documents().iterator()
        
    def __nonzero__(self):
        return (len(self) > 0)


class ViewResults(object):
    """
    Object to retrieve view results.
    """

    def __init__(self, view, **params):
        """
        Constructor of ViewResults object
        
        @param view: Object inherited from :mod:`couchapp.client.view.ViewInterface
        @param params: params to apply when fetching view.
        
        """
        self.view = view
        self.params = params
        self._result_cache = None
        self._total_rows = None
        self._offset = 0
        self._dynamic_keys = []

    def iterator(self):
        self._fetch_if_needed()
        rows = self._result_cache.get('rows', [])
        wrapper = self.view._wrapper
        for row in rows:
            if  wrapper is not None:
                yield self.view._wrapper(row)
            else:
                yield row
                    
    def first(self):
        """
        Return the first result of this query or None if the result doesn’t contain any row.

        This results in an execution of the underlying query.
        """
        
        try:
            return list(self)[0]
        except IndexError:
            return None
    
    def one(self, except_all=False):
        """
        Return exactly one result or raise an exception.
        
        
        Raises `couchapp.exceptions.MultipleResultsFound` if multiple rows are returned.
        If except_all is True, raises `couchapp.exceptions.NoResultFound` 
        if the query selects no rows. 

        This results in an execution of the underlying query.
        """
        
        length = len(self)
        if length > 1:
            raise MultipleResultsFound("%s results found." % length)

        result = self.first()
        if result is None and except_all:
            raise NoResultFound
        return result

    def all(self):
        """ return list of all results """
        return list(self.iterator())

    def count(self):
        """ return number of returned results """
        self._fetch_if_needed()
        return len(self._result_cache.get('rows', []))

    def fetch(self):
        """ fetch results and cache them """
        # reset dynamic keys
        for key in  self._dynamic_keys:
            try:
                delattr(self, key)
            except:
                pass
        self._dynamic_keys = []
        
        self._result_cache = self.view._exec(**self.params)
        self._total_rows = self._result_cache.get('total_rows')
        self._offset = self._result_cache.get('offset', 0)
        
        # add key in view results that could be added by an external
        # like couchdb-lucene
        for key in self._result_cache.keys():
            if key not in ["total_rows", "offset", "rows"]:
                self._dynamic_keys.append(key)
                setattr(self, key, self._result_cache[key])
                
                
    def fetch_raw(self):
        """ retrive the raw result """
        return self.view._exec(_raw_json=True, **self.params)

    def _fetch_if_needed(self):
        if not self._result_cache:
            self.fetch()

    @property
    def total_rows(self):
        """ return number of total rows in the view """
        self._fetch_if_needed()
        # reduce case, count number of lines
        if self._total_rows is None:
            return self.count()
        return self._total_rows

    @property
    def offset(self):
        """ current position in the view """
        self._fetch_if_needed() 
        return self._offset
        
    def __getitem__(self, key):
        params = self.params.copy()
        if type(key) is slice:
            if key.start is not None:
                params['startkey'] = key.start
            if key.stop is not None:
                params['endkey'] = key.stop
        elif isinstance(key, (list, tuple,)):
            params['keys'] = key
        else:
            params['key'] = key
        
        return ViewResults(self.view, **params)
        
    def __iter__(self):
        return self.iterator()

    def __len__(self):
        return self.count()

    def __nonzero__(self):
        return bool(len(self))
        
        
class ViewInterface(object):
    """ Generic object interface used by View and TempView objects. """
    
    def __init__(self, db, wrapper=None):
        self._db = db
        self._wrapper = wrapper
        
    def __call__(self, **params):
        return ViewResults(self, **params)
        
    def __iter__(self):
        return self()
        
    def _exec(self, **params):
        raise NotImplementedError
        
class View(ViewInterface):
    """ Object used to wrap a view and return ViewResults. Generally called. """
    
    def __init__(self, db, view_path, wrapper=None):
        ViewInterface.__init__(self, db, wrapper=wrapper)
        self.view_path = view_path
              
    def _exec(self, **params):
        if 'keys' in params:
            keys = params.pop('keys')
            return self._db.res.post(self.view_path, payload={ 'keys': keys }, **params)
        else:
            return self._db.res.get(self.view_path, **params)
            
class TempView(ViewInterface):
    """ Object used to wrap a temporary and return ViewResults. """
    def __init__(self, db, design, wrapper=None):
        ViewInterface.__init__(self, db, wrapper=wrapper)
        self.design = design
        self._wrapper = wrapper

    def _exec(self, **params):
        return self._db.res.post('_temp_view', payload=self.design,
                **params)