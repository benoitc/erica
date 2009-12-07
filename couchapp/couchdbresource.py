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
import collections
import httplib
import mimetypes
import os
import urllib
import urlparse
import re
import StringIO
import socket
import threading
import time
import types

try:
    import json
except ImportError:
    import couchapp.simplejson as json 

url_parser = urlparse.urlparse

from couchapp import __version__
from couchapp.errors import *
from couchapp.utils import to_bytestring


USER_AGENT = "couchapp/%s" % __version__
DEFAULT_TIMEOUT = 150 
MAX_CONNECTIONS = 4
DEFAULT_UUID_BATCH_COUNT = 1000
STREAM_SIZE = 16384

_http_pool = {}

class CouchDBResource(object):
  
    def __init__(self, ui, url="http://127.0.0.1:5984", headers=None, timeout=DEFAULT_TIMEOUT, 
            max_connections=MAX_CONNECTIONS, key_file=None, cert_file=None):
        headers = headers or {}
        uri = url_parser(url)
        if uri.scheme != "http" and uri.scheme != "https":
            raise ValueError('Invalid dbstring')
        if uri.username is not None:
            password = uri.password or ""
            headers['Authorization'] = 'Basic ' + base64.encodestring("%s:%s" % (uri.username, password))[:-1]
        headers.setdefault('Accept', 'application/json')
        headers.setdefault('User-Agent', USER_AGENT)
        
        self.headers = headers
        self._headers = headers
        self.ui = ui
        self.url = url
        self.max_connections = max_connections
        self.timeout = timeout
        self.key_file = key_file
        self.cert_file = cert_file
        
    def clone(self):
        """if you want to add a path to resource uri, you can do:

        .. code-block:: python

        resr2 = res.clone()
        """
        return self.__class__(self.ui, self.url, headers=self.headers,
                        timeout=self.timeout, max_connections=self.max_connections,
                        key_file=self.key_file, cert_file=self.cert_file)
    
        
    def __call__(self, path):
        """if you want to add a path to resource uri, you can do:
        
        .. code-block:: python

            CouchDBResource("/path").get()
            
        """
        
        url = make_uri(self.url, path)
        return type(self)(self.ui, url, headers=self.headers,
                        timeout=self.timeout, max_connections=self.max_connections,
                        key_file=self.key_file, cert_file=self.cert_file)
        
    def get(self, path=None, headers=None, **params):
        return self.request('GET', path=path, headers=headers, **params)
        
    def head(self, path=None, headers=None, **params):
        return self.request('HEAD', path=path, headers=headers, **params)
    
    def delete(self, path=None, headers=None, **params):
        return self.request('DELETE', path=path, headers=headers, **params)
        
    def copy(self, path=None, headers=None, **params):
        return self.request('COPY', path=path, headers=headers, **params)
        
    def post(self, path=None, payload=None, headers=None, **params) :
        return self.request('POST', path=path, payload=payload, headers=headers, **params)
     
    def put(self, path=None, payload=None, headers=None, **params) :
        return self.request('PUT', path=path, payload=payload, headers=headers, **params)
        
    def _get_pool(self, uri):
        if uri.netloc in _http_pool:
            pool = _http_pool[uri.netloc]
        else:
            pool = HTTPPool(uri, max_connections=self.max_connections,
                            key_file=self.key_file, cert_file=self.cert_file)
            _http_pool[uri.netloc] = pool
        return pool
                            
    def _get_connection(self, uri):
        pool = self._get_pool(uri)
        return pool.get()
    
    def _release_connection(self, uri, connection):
        pool = self._get_pool(uri)
        
        pool.put(connection)
        
    def _clear_pool(self, uri):
        pool = self._get_pool(uri)
        pool.clear()
    
    def _request(self, method, uri, payload=None, headers=None):
        headers = headers or self._headers
        for i in range(2):
            http = self._get_connection(uri)
            try:
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
                if payload is not None:
                    if i > 0 and hasattr(payload, 'seek'):
                        payload.seek(0)
                    if isinstance(payload, types.StringTypes) and len(payload) == 0:
                        http.send("")
                    elif isinstance(payload, types.StringTypes) or hasattr(payload, 'read'):
                        _send_body_part(payload, http)
                    elif hasattr(payload, "__iter__"):
                        for body_part in payload:
                            _send_body_part(body_part, http)
                    elif isinstance(body, list):
                        for body_part in payload:
                            _send_body_part(body_part, http)
                    else:
                        _send_body_part(payload, http)
                response = http.getresponse()
            except socket.gaierror:
                self._clear_pool(uri)
                raise RequestFailed("Unable to find the server at %s" % http.host)
            except (socket.error, httplib.HTTPException):
                self._clear_pool(uri)
                if i == 0:
                    continue
                else:
                    raise
            break
        return response, http
    
    def request(self, method, path=None, payload=None, headers=None, **params):
        stream = params.pop('_stream', False)
        raw_json = params.pop('_raw_json', False)
        
        
        headers = headers or {}
        headers.update(self._headers.copy())
        

        size = headers.get('Content-Length', None)
        if payload:
            if isinstance(payload, file):
                try:
                    payload.flush()
                except IOError:
                    pass
                size = int(os.fstat(payload.fileno())[6])
            elif isinstance(payload, types.StringTypes):
                payload = to_bytestring(payload)
                size = len(payload)
            elif not hasattr(payload, 'read') and not isinstance(payload, basestring):
                payload = json.dumps(payload).encode('utf-8')
                headers.setdefault('Content-Type', 'application/json')
                size = len(payload)
            
            if payload is not None and size is not None:
                headers.setdefault('Content-Length', size)
                
            if 'Content-Type' not in headers:
                type_ = None
                if hasattr(payload, 'name'):
                    type_ = mimetypes.guess_type(payload.name)[0]
                headers['Content-Type'] = type_ and type_ or 'application/octet-stream'
            
        headers = _normalize_headers(headers)
        uri = url_parser(make_uri(self.url, path, **encode_params(params)))
        resp, connection = self._request(method, uri, payload=payload, headers=headers)
        status_code = int(resp.status)
        
        self.response = HTTPResponse(resp)
        
        if status_code >= 400:
            data = resp.read()
            self._release_connection(uri, connection)
            if status_code == 404:
                raise ResourceNotFound(data)
            elif status_code == 401 or status_code == 403:
                raise Unauthorized(data)
            elif status_code == 409:
                raise ResourceConflict(data)
            elif status_code == 412:
                raise PreconditionFailed(data)
            else:
                raise RequestFailed("Error %s: %s" % (status_code, data))

        if raw_json:
            if stream:
                return ResponseStream(resp, uri, connection, self._release_connection)
            data = resp.read()
            self._release_connection(uri, connection)
            return _utf8(data)
        elif stream:
            return ResponseStream(resp, uri, connection, self._release_connection)
            
        data = resp.read()
        self._release_connection(uri, connection)
        data = _utf8(data)
        if resp.getheader('content-type') == 'application/json':
            try:
                data = json.loads(data)
            except ValueError:
                self.ui.logger.error("can't deserialize response on %s" % uri.geturl())
                pass
        return data
    
    def update_uri(self, path):
        """
        to set a new uri absolute path
        """
        self.url = make_uri(self.url, path)
        
class HTTPPool(object):
    
    def __init__(self, uri, max_connections=MAX_CONNECTIONS, 
            timeout=DEFAULT_TIMEOUT, key_file=None, cert_file=None):
        self.uri = uri
        self.max_connections = max_connections
        self.timeout = DEFAULT_TIMEOUT
        self.connections = collections.deque()
        self.lock = threading.Lock()
        self.key_file = key_file
        self.cert_file = cert_file
        
        
    def make_connection(self):
        kwargs = {}
        if hasattr(httplib.HTTPConnection, 'timeout'):
            kwargs['timeout'] = self.timeout
        
        if self.uri.port:
            kwargs['port'] = self.uri.port

        if self.uri.scheme == "https":
            kwargs.update(dict(key_file=self.key_file, cert_file=self.cert_file))
            connection = httplib.HTTPSConnection(self.uri.hostname, **kwargs)
        else:
            connection = httplib.HTTPConnection(self.uri.hostname, **kwargs)
            
        setattr(connection, "started", time.time())
        return connection
        
    def do_get(self):
        if self.connections:
            connection = self.connections.popleft()
            return connection
        else:
            return self.make_connection()

    def get(self):
        while True:
            connection = self.do_get()
            since = time.time() - connection.started
            if since < self.timeout:
                if connection._HTTPConnection__response:
                    connection._HTTPConnection__response.read()
                return connection
            else:
                connection.close()
    
    def put(self, connection):
        if len(self.connections) >= self.max_connections:
            connection.close()
            return
        if connection.sock is None:
            connection = self.make_connection()
        self.connections.append(connection)
        
    def clear(self):
        while self.connections:
            connection = self.connections.pop()
            connection.close()
            
class HTTPResponse(dict):
    def __init__(self, response):
        status = 200
        reason = "Ok"
        
        #populate headers and response info
        for key, value in response.getheaders():
            self[key.lower()] = value
        self.status = response.status
        self.reason = response.reason
    
    def __getattr__(self, name):
        if name == 'dict':
            return self 
        else:  
            raise AttributeError, name

    def __repr__(self):
        return "<%s [%s %s]>" % (self.__class__.__name__,
                                    self.status, self.reason)
            
class ResponseStream(object):

    def __init__(self, response, uri, connection, release_callback):
        self.resp = response
        self.uri = uri
        self.conn = connection
        self.release_callback = release_callback
        self.stream_size = STREAM_SIZE

   
    def read(self, amt=None):
        data = None
        if not self.resp.isclosed():
            data = self.resp.read(amt)
        if not data:
            self.release_callback(self.uri, self.conn)
        return data
        
    def close(self):
        if not self.resp.isclosed():
            self.resp.close()
        
    def next(self):
        data = self.read(self.stream_size)
        if not data:
            raise StopIteration()
        return data
        
    def __iter__(self):
        return self


def _utf8(s):
    """ return an unicode """
    try:
        try:
            return unicode(s)
        except UnicodeDecodeError:
            return unicode(s.decode('utf-8'))
    except:
        return s


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


def make_uri(base, *path, **qs):
    """Assemble a uri based on a base, any number of path segments, and query
    string parameters.
    
    @params path: subpaths
    @params params: list of params
    @params _charset: url charset
    @params _safe: additional characters that should not be quoted 

    """
    charset = qs.pop('_charset', 'utf-8')
    safe = qs.pop('_safe', ':%/')
    encode_keys = qs.pop('_encode_keys', True)
    
    base_trailing_slash = False
    if base and base.endswith("/"):
        base_trailing_slash = True
        base = base[:-1]
    retval = [base]

    # build the path
    _path = []
    trailing_slash = False       
    for s in path:
        if s is not None and isinstance(s, basestring):
            if len(s) > 1 and s.endswith('/'):
                trailing_slash = True
            else:
                trailing_slash = False
            _path.append(url_quote(s.strip('/'), charset, safe))
                   
    path_str =""
    if _path:
        path_str = "/".join([''] + _path)
        if trailing_slash:
            path_str = path_str + "/" 
    elif base_trailing_slash:
        path_str = path_str + "/" 
        
    if path_str:
        retval.append(path_str) 

    params = []
    for k, v in qs.items():
        if v is None:
            continue
        if type(v) in (list, tuple):
            params.extend([(k, i) for i in v if i is not None])
        elif v is not None:
            params.append((k,v))
    if params:
        retval.extend(['?', url_encode(dict(params), charset, encode_keys)])

    return ''.join(retval)


def url_quote(s, charset='utf-8', safe='/:'):
    """URL encode a single string with a given encoding."""
    if isinstance(s, unicode):
        s = s.encode(charset)
    elif not isinstance(s, str):
        s = str(s)
    return urllib.quote(s, safe=safe)


def url_encode(obj, charset="utf8", encode_keys=False):
    if isinstance(obj, dict):
        items = []
        for k, v in obj.iteritems():
            if not isinstance(v, (tuple, list)):
                v = [v]
            items.append((k, v))
    else:
        items = obj or ()

    tmp = []
    for key, values in items:
        if encode_keys and isinstance(key, unicode):
            key = key.encode(charset)
        else:
            key = str(key)

        for value in values:
            if value is None:
                continue
            elif isinstance(value, unicode):
                value = value.encode(charset)
            else:
                value = str(value)
        tmp.append('%s=%s' % (urllib.quote(key),
            urllib.quote_plus(value)))

    return '&'.join(tmp)
