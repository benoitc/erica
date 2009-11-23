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

import base64
import httplib
import mimetypes
import os
import urllib
import urlparse
import re
import StringIO
import socket
import types

try:
    import json
except ImportError:
    import simplejson as json 

url_parser = urlparse.urlparse

__all__ = ['is_doc', 'get_doc', 'save_doc', 'put_attachment', 'fetch_attachment',
            'delete_attachment']

from couchapp import __version__
from couchapp.errors import *
from couchapp.utils import to_bytestring


http_pool = {}
USER_AGENT = "couchapp/%s" % __version__

def create_db(dbstring):
    make_request(dbstring, "PUT")
    

def is_doc(dbstring, docid):
    """ if a doc with docid exist in db """
    url = "%s/%s" % (dbstring, escape_docid(docid))
    resp = make_request(url, "GET")
    try:
        parse_resp(resp)
    except:
        return False
    return True
    

def get_doc(dbstring, docid, **params):
    """ get document with docid
    
    @param dbstring: str, db url
    @param docid: str, doc id
    
    @return: dict, the document dict
    """
    params_str = ""
    if params:
        params = encode_params(params)
        params_str = "".join(["%s=%s" % (url_quote(k), url_quote(v)) \
                            for k,v in params.items() \
                            if v and v is not None])
                            
        if params_str: params_str = "?%s" % params_str
    
    uri = "%s/%s%s" % (dbstring, escape_docid(docid), params_str)
    
    print uri
    resp = make_request(uri, "GET")
    parse_resp(resp)
    data = resp.read()
    return json.loads(data)
    
def save_doc(dbstring, doc, encode=False):
    """ save doc
    
    @param dbstring: db url
    @param doc: dict, the document dict
    
    """
    if '_attachments' in doc and encode:
        doc['_attachments'] = encode_attachments(doc['_attachments'])
    docid = escape_docid(doc['_id'])
    json_doc = json.dumps(doc).encode("utf-8")
    headers =  {
        "Content-Type": "application/json",
        "Content-Length": str(len(json_doc))
    }
    
    resp = make_request("%s/%s" % (dbstring, docid), "PUT", 
                body=json_doc, headers=headers)
    parse_resp(resp)
    res = json.loads(resp.read())
    doc.update({ '_id': res['id'], '_rev': res['rev']})
    
def save_docs(dbstring, docs):
    """ bulk save. Modify Multiple Documents With a Single Request
    @param dbstring: db url
    @param docs: list of docs
  
    .. seealso:: `HTTP Bulk Document API <http://wiki.apache.org/couchdb/HTTP_Bulk_Document_API>`
    
    """
    # we definitely need a list here, not any iterable, or groupby will fail
    docs = list(docs)    
    for doc in docs:
        doc['_id'] = escape_docid(doc['_id'])
        
        
    payload = { "docs": docs }
    json_docs = json.dumps(payload).encode("utf-8")
    headers =  {
        "Content-Type": "application/json",
        "Content-Length": str(len(json_docs))
    }
    # update docs
    resp = make_request("%s/_bulk_docs" % dbstring, "POST", 
                body=json_docs, headers=headers)
    parse_resp(resp)
    results = json.loads(resp.read())
       
    errors = []
    for i, res in enumerate(results):
        if 'error' in res:
            errors.append(res)
        else: 
            docs[i].update({'_id': res['id'], '_rev': res['rev']})
    if errors:
        raise BulkSaveError(errors)
    
    
 
def put_attachment(dbstring, doc, content, name=None, 
        content_type=None, content_length=None):
    """ Add attachement to a document. All attachments are streamed.

    @param dbstring: db url
    @param doc: dict, document object
    @param content: string or :obj:`File` object.
    @param name: name or attachment (file name).
    @param content_type: string, mimetype of attachment.
    If you don't set it, it will be autodetected.
    @param content_lenght: int, size of attachment.

    @return: bool, True if everything was ok.
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
    name = url_quote(name, safe="")
    if content_type is None:
        content_type = ';'.join(filter(None, mimetypes.guess_type(name)))

    if content_type:
        headers['Content-Type'] = content_type
        
    # add appropriate headers    
    if content_length and content_length is not None:
        headers['Content-Length'] = content_length

    docid = escape_docid(doc['_id'])
    resp = make_request("%s/%s/%s?rev=%s" % (dbstring, doc['_id'], name, doc['_rev']), 
                "PUT", body=content, headers=headers)

    parse_resp(resp)
    res = json.loads(resp.read())
    if res['ok']:
        doc.update({ '_rev': res['rev']})
    return res['ok']
 
def fetch_attachment(dbstring, id_or_doc, name):
    """ get attachment in a document
    
    @param dbstring: db url
    @param id_or_doc: str or dict, doc id or document dict
    @param name: name of attachment default: default result
    
    @return: HTTPResponse instance.
    """
    if isinstance(id_or_doc, basestring):
        docid = id_or_doc
    else:
        docid = id_or_doc['_id']
  
    docid = escape_docid(docid)
    name = url_quote(name, safe="")
    resp = make_request("%s/%s/%s" % (dbstring, docid, name), "GET")
    parse_resp(resp)
    return resp

def delete_attachment(dbstring, doc, name):
    """ delete attachement to the document

    @param dbstring: db url
    @param doc: dict, document object in python
    @param name: name of attachement
    
    @return: boolean, True if delete was ok.
    """
    docid = escape_docid(doc['_id'])
    name = url_quote(name, safe="")
    resp = make_request("%s/%s/%s?rev=%s" % (dbstring, docid, name, doc['_rev']), "DELETE")
    parse_resp(resp)
    res = resp.read()
    if res['ok']:
        doc.update({ '_rev': res['rev']})
    return res['ok']



def parse_resp(resp):
    status_code = int(resp.status)
    if status_code >= 400:
        data = resp.read()
        if status_code == 404:
            raise ResourceNotFound(data)
        elif status_code == 401 or status_code == 403:
            raise Unauthorized(data)
        elif status_code == 409:
            raise ResourceConflict(data)
        elif status_code == 412:
            raise PreconditionFailed(data)
        else:
            raise RequestFailed("Error %s: %s" % (resp.status, data))

def make_request(url, method, body=None, headers=None):
    headers = headers or {}
    uri = url_parser(urllib.unquote(url))
    if uri.scheme != "http" and uri.scheme != "https":
        raise ValueError('Invalid dbstring')
    if uri.username is not None:
        password = uri.password or ""
        headers['Authorization'] = 'Basic ' + base64.encodestring("%s:%s" % (uri.username, password))[:-1]
    
    if isinstance(body, file):
        try:
            body.flush()
        except IOError:
            pass
        size = int(os.fstat(body.fileno())[6])
    elif isinstance(body, types.StringTypes):
        body = to_bytestring(body)
        size = len(body)
      
    if body is not None:
        headers.setdefault('Content-Length', size)
        
    headers.setdefault('Accept', 'application/json')
    headers.setdefault('User-Agent', USER_AGENT)
    headers = _normalize_headers(headers)
    
    
    http = _get_connection(uri.netloc)
    for i in range(2):
        try:
            if http._HTTPConnection__response:
                http._HTTPConnection__response.read()
            # init connection
            if http.host != uri.hostname:
                http.putrequest(method, uri.geturl())
            else:
                http.putrequest(method, _relative_uri(uri))
         
            # Send the HTTP headers.
            for header_name, value in headers.iteritems():
                http.putheader(header_name, value)
            http.endheaders()
            
            # send body
            if body is not None:
                if i > 0 and hasattr(body, 'seek'):
                    body.seek(0)
                if isinstance(body, types.StringTypes) and len(body) == 0:
                    http.send("")
                elif isinstance(body, types.StringTypes) or hasattr(body, 'read'):
                    _send_body_part(body, http)
                elif hasattr(body, "__iter__"):
                    for body_part in body:
                        _send_body_part(body_part, http)
                elif isinstance(body, list):
                    for body_part in body:
                        _send_body_part(body_part, http)
                else:
                    _send_body_part(body, http)
        except socket.gaierror:
            http.close()
            raise RequestFailed("Unable to find the server at %s" % http.host)
        except (socket.error, httplib.HTTPException):
            http.close()
            if i == 0:
                continue
            else:
                raise        
        try:
            response = http.getresponse()
        except httplib.HTTPException:
            http.close()
            if i == 0:
                continue
            else:
                raise
        break
    return response
    
    

NORMALIZE_SPACE = re.compile(r'(?:\r\n)?[ \t]+')
def _normalize_headers(headers):
    return dict([ (key.lower(), NORMALIZE_SPACE.sub(str(value), ' ').strip())  for (key, value) in headers.iteritems()])

def _relative_uri(uri):
    if not uri.path:
        path = "/"
    else:
        path = uri.path
    if uri.query:
        return path + "?" + uri.query
    return path


def _get_connection(netloc):
    if netloc in http_pool:
        http = http_pool[netloc]
        if http._HTTPConnection__response:
            http._HTTPConnection__response.read()
    else:
        http = httplib.HTTPConnection(netloc)
        http_pool[netloc] = http
    return http
        
       
def _send_body_part(data, connection):
    if isinstance(data, types.StringTypes):
        data = StringIO.StringIO(to_bytestring(data))
    elif not hasattr(data, 'read'):
        data = StringIO.StringIO(str(data))
    
    # we always stream
    while 1:
        binarydata = data.read(16384)
        if binarydata == '': break
        connection.send(binarydata)
        
def encode_params(params):
    """ encode parameters in json if needed """
    _params = {}
    if params:
        for name, value in params.items():
            if name in ('key', 'startkey', 'endkey') \
                    or not isinstance(value, basestring):
                value = json.dumps(value).encode('utf-8')
            _params[name] = value
    return _params    
    
def escape_docid(docid):
    if docid.startswith('/'):
        docid = docid[1:]
    if docid.startswith('_design'):
        docid = '_design/%s' % url_quote(docid[8:], safe='')
    else:
        docid = url_quote(docid, safe='')
    return docid
    
def encode_attachments(attachments):
    for k, v in attachments.iteritems():
        if v.get('stub', False):
            continue
        else:
            re_sp = re.compile('\s')
            v['data'] = re_sp.sub('', base64.b64encode(v['data']))
    return attachments

def url_quote(s, charset='utf-8', safe='/:'):
    """URL encode a single string with a given encoding."""
    if isinstance(s, unicode):
        s = s.encode(charset)
    elif not isinstance(s, str):
        s = str(s)
    return urllib.quote(s, safe=safe)


    