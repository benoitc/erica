# -*- coding: utf-8 -*-
#
# Copyright (C) 2007-2008 Christopher Lenz
# All rights reserved.
#
# This software is licensed as described in the file COPYING, which
# you should have received as part of this distribution.

"""Python client API for CouchDB.

>>> server = Server('http://localhost:5984/')
>>> db = server.create('python-tests')
>>> doc_id = db.create({'type': 'Person', 'name': 'John Doe'})
>>> doc = db[doc_id]
>>> doc['type']
'Person'
>>> doc['name']
'John Doe'
>>> del db[doc.id]
>>> doc.id in db
False

>>> del server['python-tests']
"""

from couchapp.contrib import httplib2
import mimetypes
from urllib import quote, urlencode
from types import FunctionType
from inspect import getsource
from textwrap import dedent
import re
import socket

try:
    import json
except ImportError:
    from couchapp.contrib import simplejson as json

__all__ = ['PreconditionFailed', 'ResourceNotFound', 'ResourceConflict',
           'ServerError', 'Server', 'Database', 'Document', 'ViewResults',
           'Row']
__docformat__ = 'restructuredtext en'


DEFAULT_BASE_URI = 'http://localhost:5984/'


class PreconditionFailed(Exception):
    """Exception raised when a 412 HTTP error is received in response to a
    request.
    """


class ResourceNotFound(Exception):
    """Exception raised when a 404 HTTP error is received in response to a
    request.
    """


class ResourceConflict(Exception):
    """Exception raised when a 409 HTTP error is received in response to a
    request.
    """


class ServerError(Exception):
    """Exception raised when an unexpected HTTP error is received in response
    to a request.
    """


class Server(object):
    """Representation of a CouchDB server.

    >>> server = Server('http://localhost:5984/')

    This class behaves like a dictionary of databases. For example, to get a
    list of database names on the server, you can simply iterate over the
    server object.

    New databases can be created using the `create` method:

    >>> db = server.create('python-tests')
    >>> db
    <Database 'python-tests'>

    You can access existing databases using item access, specifying the database
    name as the key:

    >>> db = server['python-tests']
    >>> db.name
    'python-tests'

    Databases can be deleted using a ``del`` statement:

    >>> del server['python-tests']
    """

    def __init__(self, uri=DEFAULT_BASE_URI, cache=None, timeout=None):
        """Initialize the server object.
        
        :param uri: the URI of the server (for example
                    ``http://localhost:5984/``)
        :param cache: either a cache directory path (as a string) or an object
                      compatible with the ``httplib2.FileCache`` interface. If
                      `None` (the default), no caching is performed.
        :param timeout: socket timeout in number of seconds, or `None` for no
                        timeout
        """
        http = httplib2.Http(cache=cache, timeout=timeout)
        http.force_exception_to_status_code = False
        self.resource = Resource(http, uri)

    def __contains__(self, name):
        """Return whether the server contains a database with the specified
        name.

        :param name: the database name
        :return: `True` if a database with the name exists, `False` otherwise
        """
        try:
            self.resource.head(validate_dbname(name))
            return True
        except ResourceNotFound:
            return False

    def __iter__(self):
        """Iterate over the names of all databases."""
        resp, data = self.resource.get('_all_dbs')
        return iter(data)

    def __len__(self):
        """Return the number of databases."""
        resp, data = self.resource.get('_all_dbs')
        return len(data)

    def __nonzero__(self):
        """Return whether the server is available."""
        try:
            self.resource.head()
            return True
        except:
            return False

    def __repr__(self):
        return '<%s %r>' % (type(self).__name__, self.resource.uri)

    def __delitem__(self, name):
        """Remove the database with the specified name.

        :param name: the name of the database
        :raise ResourceNotFound: if no database with that name exists
        """
        self.resource.delete(validate_dbname(name))

    def __getitem__(self, name):
        """Return a `Database` object representing the database with the
        specified name.

        :param name: the name of the database
        :return: a `Database` object representing the database
        :rtype: `Database`
        :raise ResourceNotFound: if no database with that name exists
        """
        return Database(uri(self.resource.uri, name), validate_dbname(name),
                        http=self.resource.http)

    def _get_config(self):
        resp, data = self.resource.get('_config')
        return data
    config = property(_get_config, doc="""\
        The configuration of the CouchDB server.

        The configuration is represented as a nested dictionary of sections and
        options from the configuration files of the server, or the default
        values for options that are not explicitly configured.

        :type: `dict`
        """)

    def _get_version(self):
        resp, data = self.resource.get()
        return data['version']
    version = property(_get_version, doc="""\
        The version number tuple for the CouchDB server.

        Note that this results in a request being made, and can also be used
        to check for the availability of the server.
        
        :type: `unicode`
        """)

    def create(self, name):
        """Create a new database with the given name.

        :param name: the name of the database
        :return: a `Database` object representing the created database
        :rtype: `Database`
        :raise ResourceConflict: if a database with that name already exists
        """
        self.resource.put(validate_dbname(name))
        return self[name]


class Database(object):
    """Representation of a database on a CouchDB server.

    >>> server = Server('http://localhost:5984/')
    >>> db = server.create('python-tests')

    New documents can be added to the database using the `create()` method:

    >>> doc_id = db.create({'type': 'Person', 'name': 'John Doe'})

    This class provides a dictionary-like interface to databases: documents are
    retrieved by their ID using item access

    >>> doc = db[doc_id]
    >>> doc                 #doctest: +ELLIPSIS
    <Document '...'@... {...}>

    Documents are represented as instances of the `Row` class, which is
    basically just a normal dictionary with the additional attributes ``id`` and
    ``rev``:

    >>> doc.id, doc.rev     #doctest: +ELLIPSIS
    ('...', ...)
    >>> doc['type']
    'Person'
    >>> doc['name']
    'John Doe'

    To update an existing document, you use item access, too:

    >>> doc['name'] = 'Mary Jane'
    >>> db[doc.id] = doc

    The `create()` method creates a document with an auto-generated ID. If you
    want to explicitly specify the ID, you'd use item access just as with
    updating:

    >>> db['JohnDoe'] = {'type': 'person', 'name': 'John Doe'}

    >>> 'JohnDoe' in db
    True
    >>> len(db)
    2

    >>> del server['python-tests']
    """

    def __init__(self, uri, name=None, http=None):
        self.resource = Resource(http, uri)
        self._name = name

    def __repr__(self):
        return '<%s %r>' % (type(self).__name__, self.name)

    def __contains__(self, id):
        """Return whether the database contains a document with the specified
        ID.

        :param id: the document ID
        :return: `True` if a document with the ID exists, `False` otherwise
        """
        try:
            self.resource.head(id)
            return True
        except ResourceNotFound:
            return False

    def __iter__(self):
        """Return the IDs of all documents in the database."""
        return iter([item.id for item in self.view('_all_docs')])

    def __len__(self):
        """Return the number of documents in the database."""
        resp, data = self.resource.get()
        return data['doc_count']

    def __nonzero__(self):
        """Return whether the database is available."""
        try:
            self.resource.head()
            return True
        except:
            return False

    def __delitem__(self, id):
        """Remove the document with the specified ID from the database.

        :param id: the document ID
        """
        resp, data = self.resource.head(id)
        self.resource.delete(id, rev=resp['etag'].strip('"'))

    def __getitem__(self, id):
        """Return the document with the specified ID.

        :param id: the document ID
        :return: a `Row` object representing the requested document
        :rtype: `Document`
        """
        resp, data = self.resource.get(id)
        return Document(data)

    def __setitem__(self, id, content):
        """Create or update a document with the specified ID.

        :param id: the document ID
        :param content: the document content; either a plain dictionary for
                        new documents, or a `Row` object for existing
                        documents
        """
        resp, data = self.resource.put(id, content=content)
        content.update({'_id': data['id'], '_rev': data['rev']})

    def _get_name(self):
        if self._name is None:
            self._name = self.info()['db_name']
        return self._name
    name = property(_get_name)

    def create(self, data):
        """Create a new document in the database with a generated ID.

        Any keyword arguments are used to populate the fields of the new
        document.

        :param data: the data to store in the document
        :return: the ID of the created document
        :rtype: `unicode`
        """
        resp, data = self.resource.post(content=data)
        return data['id']

    def compact(self):
        """Compact the database.

        This will try to prune all revisions from the database.

        :return: a boolean to indicate whether the compaction was initiated
                 successfully
        :rtype: `bool`
        """
        resp, data = self.resource.post('_compact')
        return data['ok']

    def delete(self, doc):
        """Delete the given document from the database.

        Use this method in preference over ``__del__`` to ensure you're
        deleting the revision that you had previously retrieved. In the case
        the document has been updated since it was retrieved, this method will
        raise a `ResourceConflict` exception.

        >>> server = Server('http://localhost:5984/')
        >>> db = server.create('python-tests')

        >>> doc = dict(type='Person', name='John Doe')
        >>> db['johndoe'] = doc
        >>> doc2 = db['johndoe']
        >>> doc2['age'] = 42
        >>> db['johndoe'] = doc2
        >>> db.delete(doc)
        Traceback (most recent call last):
          ...
        ResourceConflict: ('conflict', 'Document update conflict.')

        >>> del server['python-tests']
        
        :param doc: a dictionary or `Document` object holding the document data
        :raise ResourceConflict: if the document was updated in the database
        :since: 0.4.1
        """
        self.resource.delete(doc['_id'], rev=doc['_rev'])

    def get(self, id, default=None, **options):
        """Return the document with the specified ID.

        :param id: the document ID
        :param default: the default value to return when the document is not
                        found
        :return: a `Row` object representing the requested document, or `None`
                 if no document with the ID was found
        :rtype: `Document`
        """
        try:
            resp, data = self.resource.get(id, **options)
        except ResourceNotFound:
            return default
        else:
            return Document(data)

    def info(self):
        """Return information about the database as a dictionary.
        
        The returned dictionary exactly corresponds to the JSON response to
        a ``GET`` request on the database URI.
        
        :return: a dictionary of database properties
        :rtype: ``dict``
        :since: 0.4
        """
        resp, data = self.resource.get()
        return data

    def delete_attachment(self, doc, filename):
        """Delete the specified attachment.
        
        Note that the provided `doc` is required to have a `_rev` field. Thus,
        if the `doc` is based on a view row, the view row would need to include
        the `_rev` field.

        :param doc: the dictionary or `Document` object representing the
                    document that the attachment belongs to
        :param filename: the name of the attachment file
        :since: 0.4.1
        """
        resp, data = self.resource(doc['_id']).delete(filename, rev=doc['_rev'])
        doc['_rev'] = data['rev']

    def get_attachment(self, id_or_doc, filename, default=None):
        """Return an attachment from the specified doc id and filename.
        
        :param id_or_doc: either a document ID or a dictionary or `Document`
                          object representing the document that the attachment
                          belongs to
        :param filename: the name of the attachment file
        :param default: default value to return when the document or attachment
                        is not found
        :return: the content of the attachment as a string, or the value of the
                 `default` argument if the attachment is not found
        :since: 0.4.1
        """
        if isinstance(id_or_doc, basestring):
            id = id_or_doc
        else:
            id = id_or_doc['_id']
        try:
            resp, data = self.resource(id).get(filename)
            return data
        except ResourceNotFound:
            return default

    def put_attachment(self, doc, content, filename=None, content_type=None, 
        content_length=None):
        """Create or replace an attachment.

        Note that the provided `doc` is required to have a `_rev` field. Thus,
        if the `doc` is based on a view row, the view row would need to include
        the `_rev` field.

        :param doc: the dictionary or `Document` object representing the
                    document that the attachment should be added to
        :param content: the content to upload, either a file-like object or
                        a string
        :param filename: the name of the attachment file; if omitted, this
                         function tries to get the filename from the file-like
                         object passed as the `content` argument value
        :param content_type: content type of the attachment; if omitted, the
                             MIME type is guessed based on the file name
                             extension
        :since: 0.4.1
        """
        headers = {}
        
        if hasattr(content, 'read'):
            content = content.read()
        if filename is None:
            if hasattr(content, 'name'):
                filename = content.name
            else:
                raise ValueError('no filename specified for attachment')
        if content_type is None:
            content_type = ';'.join(filter(None, mimetypes.guess_type(filename)))

        if content_type:
            headers['Content-Type'] = content_type

        if content_length and content_length is not None:
            headers['Content-Length'] = str(content_length)

        resp, data = self.resource(doc['_id']).put(filename, content=content,
                            headers=headers, rev=doc['_rev'])
        doc['_rev'] = data['rev']

    def query(self, map_fun, reduce_fun=None, language='javascript',
              wrapper=None, **options):
        """Execute an ad-hoc query (a "temp view") against the database.
        
        >>> server = Server('http://localhost:5984/')
        >>> db = server.create('python-tests')
        >>> db['johndoe'] = dict(type='Person', name='John Doe')
        >>> db['maryjane'] = dict(type='Person', name='Mary Jane')
        >>> db['gotham'] = dict(type='City', name='Gotham City')
        >>> map_fun = '''function(doc) {
        ...     if (doc.type == 'Person')
        ...         emit(doc.name, null);
        ... }'''
        >>> for row in db.query(map_fun):
        ...     print row.key
        John Doe
        Mary Jane
        
        >>> for row in db.query(map_fun, descending=True):
        ...     print row.key
        Mary Jane
        John Doe
        
        >>> for row in db.query(map_fun, key='John Doe'):
        ...     print row.key
        John Doe
        
        >>> del server['python-tests']
        
        :param map_fun: the code of the map function
        :param reduce_fun: the code of the reduce function (optional)
        :param language: the language of the functions, to determine which view
                         server to use
        :param wrapper: an optional callable that should be used to wrap the
                        result rows
        :param options: optional query string parameters
        :return: the view reults
        :rtype: `ViewResults`
        """
        return TemporaryView(uri(self.resource.uri, '_temp_view'), map_fun,
                             reduce_fun, language=language, wrapper=wrapper,
                             http=self.resource.http)(**options)

    def update(self, documents, **options):
        """Perform a bulk update or insertion of the given documents using a
        single HTTP request.
        
        >>> server = Server('http://localhost:5984/')
        >>> db = server.create('python-tests')
        >>> for doc in db.update([
        ...     Document(type='Person', name='John Doe'),
        ...     Document(type='Person', name='Mary Jane'),
        ...     Document(type='City', name='Gotham City')
        ... ]):
        ...     print repr(doc) #doctest: +ELLIPSIS
        <Document '...'@'...' {'type': 'Person', 'name': 'John Doe'}>
        <Document '...'@'...' {'type': 'Person', 'name': 'Mary Jane'}>
        <Document '...'@'...' {'type': 'City', 'name': 'Gotham City'}>
        
        >>> del server['python-tests']
        
        If an object in the documents list is not a dictionary, this method
        looks for an ``items()`` method that can be used to convert the object
        to a dictionary. In this case, the returned generator will not update
        and yield the original object, but rather yield a dictionary with
        ``id`` and ``rev`` keys.
        
        :param documents: a sequence of dictionaries or `Document` objects, or
                          objects providing a ``items()`` method that can be
                          used to convert them to a dictionary
        :return: an iterable over the resulting documents
        :rtype: ``generator``
        
        :since: version 0.2
        """
        docs = []
        for doc in documents:
            if isinstance(doc, dict):
                docs.append(doc)
            elif hasattr(doc, 'items'):
                docs.append(dict(doc.items()))
            else:
                raise TypeError('expected dict, got %s' % type(doc))
        content = options
        content.update(docs=docs)
        resp, data = self.resource.post('_bulk_docs', content=content)
        def _update():
            for idx, result in enumerate(data):
                if 'error' in result:
                    yield result
                else:
                    doc = documents[idx]
                    if isinstance(doc, dict):
                        doc.update({'_id': result['id'], '_rev': result['rev']})
                        yield doc
                    else:
                        yield result
        return _update()

    def view(self, name, wrapper=None, **options):
        """Execute a predefined view.
        
        >>> server = Server('http://localhost:5984/')
        >>> db = server.create('python-tests')
        >>> db['gotham'] = dict(type='City', name='Gotham City')
        
        >>> for row in db.view('_all_docs'):
        ...     print row.id
        gotham
        
        >>> del server['python-tests']
        
        :param name: the name of the view; for custom views, use the format
                     ``design_docid/viewname``, that is, the document ID of the
                     design document and the name of the view, separated by a
                     slash
        :param wrapper: an optional callable that should be used to wrap the
                        result rows
        :param options: optional query string parameters
        :return: the view results
        :rtype: `ViewResults`
        """
        if not name.startswith('_'):
            design, name = name.split('/', 1)
            name = '/'.join(['_design', design, '_view', name])
        return PermanentView(uri(self.resource.uri, *name.split('/')), name,
                             wrapper=wrapper,
                             http=self.resource.http)(**options)


class Document(dict):
    """Representation of a document in the database.

    This is basically just a dictionary with the two additional properties
    `id` and `rev`, which contain the document ID and revision, respectively.
    """

    def __repr__(self):
        return '<%s %r@%r %r>' % (type(self).__name__, self.id, self.rev,
                                  dict([(k,v) for k,v in self.items()
                                        if k not in ('_id', '_rev')]))

    id = property(lambda self: self['_id'])
    rev = property(lambda self: self['_rev'])


class View(object):
    """Abstract representation of a view or query."""

    def __init__(self, uri, wrapper=None, http=None):
        self.resource = Resource(http, uri)
        self.wrapper = wrapper

    def __call__(self, **options):
        return ViewResults(self, options)

    def __iter__(self):
        return self()

    def _encode_options(self, options):
        retval = {}
        for name, value in options.items():
            if name in ('key', 'startkey', 'endkey') \
                    or not isinstance(value, basestring):
                value = json.dumps(value, allow_nan=False, ensure_ascii=False)
            retval[name] = value
        return retval

    def _exec(self, options):
        raise NotImplementedError


class PermanentView(View):
    """Representation of a permanent view on the server."""

    def __init__(self, uri, name, wrapper=None, http=None):
        View.__init__(self, uri, wrapper=wrapper, http=http)
        self.name = name

    def __repr__(self):
        return '<%s %r>' % (type(self).__name__, self.name)

    def _exec(self, options):
        if 'keys' in options:
            options = options.copy()
            keys = {'keys': options.pop('keys')}
            resp, data = self.resource.post(content=keys,
                                            **self._encode_options(options))
        else:
            resp, data = self.resource.get(**self._encode_options(options))
        return data


class TemporaryView(View):
    """Representation of a temporary view."""

    def __init__(self, uri, map_fun, reduce_fun=None,
                 language='javascript', wrapper=None, http=None):
        View.__init__(self, uri, wrapper=wrapper, http=http)
        if isinstance(map_fun, FunctionType):
            map_fun = getsource(map_fun).rstrip('\n\r')
        self.map_fun = dedent(map_fun.lstrip('\n\r'))
        if isinstance(reduce_fun, FunctionType):
            reduce_fun = getsource(reduce_fun).rstrip('\n\r')
        if reduce_fun:
            reduce_fun = dedent(reduce_fun.lstrip('\n\r'))
        self.reduce_fun = reduce_fun
        self.language = language

    def __repr__(self):
        return '<%s %r %r>' % (type(self).__name__, self.map_fun,
                               self.reduce_fun)

    def _exec(self, options):
        body = {'map': self.map_fun, 'language': self.language}
        if self.reduce_fun:
            body['reduce'] = self.reduce_fun
        if 'keys' in options:
            options = options.copy()
            body['keys'] = options.pop('keys')
        content = json.dumps(body, allow_nan=False,
                             ensure_ascii=False).encode('utf-8')
        resp, data = self.resource.post(content=content, headers={
            'Content-Type': 'application/json'
        }, **self._encode_options(options))
        return data


class ViewResults(object):
    """Representation of a parameterized view (either permanent or temporary)
    and the results it produces.
    
    This class allows the specification of ``key``, ``startkey``, and
    ``endkey`` options using Python slice notation.
    
    >>> server = Server('http://localhost:5984/')
    >>> db = server.create('python-tests')
    >>> db['johndoe'] = dict(type='Person', name='John Doe')
    >>> db['maryjane'] = dict(type='Person', name='Mary Jane')
    >>> db['gotham'] = dict(type='City', name='Gotham City')
    >>> map_fun = '''function(doc) {
    ...     emit([doc.type, doc.name], doc.name);
    ... }'''
    >>> results = db.query(map_fun)

    At this point, the view has not actually been accessed yet. It is accessed
    as soon as it is iterated over, its length is requested, or one of its
    `rows`, `total_rows`, or `offset` properties are accessed:
    
    >>> len(results)
    3

    You can use slices to apply ``startkey`` and/or ``endkey`` options to the
    view:

    >>> people = results[['Person']:['Person','ZZZZ']]
    >>> for person in people:
    ...     print person.value
    John Doe
    Mary Jane
    >>> people.total_rows, people.offset
    (3, 1)
    
    Use plain indexed notation (without a slice) to apply the ``key`` option.
    Note that as CouchDB makes no claim that keys are unique in a view, this
    can still return multiple rows:
    
    >>> list(results[['City', 'Gotham City']])
    [<Row id='gotham', key=['City', 'Gotham City'], value='Gotham City'>]

    >>> del server['python-tests']
    """

    def __init__(self, view, options):
        self.view = view
        self.options = options
        self._rows = self._total_rows = self._offset = None

    def __repr__(self):
        return '<%s %r %r>' % (type(self).__name__, self.view, self.options)

    def __getitem__(self, key):
        options = self.options.copy()
        if type(key) is slice:
            if key.start is not None:
                options['startkey'] = key.start
            if key.stop is not None:
                options['endkey'] = key.stop
            return ViewResults(self.view, options)
        else:
            options['key'] = key
            return ViewResults(self.view, options)

    def __iter__(self):
        wrapper = self.view.wrapper
        for row in self.rows:
            if wrapper is not None:
                yield wrapper(row)
            else:
                yield row

    def __len__(self):
        return len(self.rows)

    def _fetch(self):
        data = self.view._exec(self.options)
        self._rows = [Row(row) for row in data['rows']]
        self._total_rows = data.get('total_rows')
        self._offset = data.get('offset', 0)

    def _get_rows(self):
        if self._rows is None:
            self._fetch()
        return self._rows
    rows = property(_get_rows, doc="""\
        The list of rows returned by the view.
        
        :type: `list`
        """)

    def _get_total_rows(self):
        if self._rows is None:
            self._fetch()
        return self._total_rows
    total_rows = property(_get_total_rows, doc="""\
        The total number of rows in this view.
        
        This value is `None` for reduce views.
        
        :type: `int` or ``NoneType`` for reduce views
        """)

    def _get_offset(self):
        if self._rows is None:
            self._fetch()
        return self._offset
    offset = property(_get_offset, doc="""\
        The offset of the results from the first row in the view.
        
        This value is 0 for reduce views.
        
        :type: `int`
        """)


class Row(dict):
    """Representation of a row as returned by database views."""

    def __repr__(self):
        if self.id is None:
            return '<%s key=%r, value=%r>' % (type(self).__name__, self.key,
                                              self.value)
        return '<%s id=%r, key=%r, value=%r>' % (type(self).__name__, self.id,
                                                 self.key, self.value)

    def _get_id(self):
        return self.get('id')
    id = property(_get_id, doc="""\
        The associated Document ID if it exists. Returns `None` when it
        doesn't (reduce results).
    """)

    def _get_key(self):
        return self['key']
    key = property(_get_key, doc='The associated key.')

    def _get_value(self):
        return self['value']
    value = property(_get_value, doc='The associated value.')

    def _get_doc(self):
        doc = self.get('doc')
        if doc:
            return Document(doc)
    doc = property(_get_doc, doc="""\
        The associated document for the row. This is only present when the
        view was accessed with ``include_docs=True`` as a query parameter,
        otherwise this property will be `None`.
    """)


# Internals


class Resource(object):

    def __init__(self, http, uri):
        if http is None:
            http = httplib2.Http()
            http.force_exception_to_status_code = False
        self.http = http
        self.uri = uri

    def __call__(self, path):
        return type(self)(self.http, uri(self.uri, path))

    def delete(self, path=None, headers=None, **params):
        return self._request('DELETE', path, headers=headers, **params)

    def get(self, path=None, headers=None, **params):
        return self._request('GET', path, headers=headers, **params)

    def head(self, path=None, headers=None, **params):
        return self._request('HEAD', path, headers=headers, **params)

    def post(self, path=None, content=None, headers=None, **params):
        return self._request('POST', path, content=content, headers=headers,
                             **params)

    def put(self, path=None, content=None, headers=None, **params):
        return self._request('PUT', path, content=content, headers=headers,
                             **params)

    def _request(self, method, path=None, content=None, headers=None,
                 **params):
        from couchapp import __version__
        headers = headers or {}
        headers.setdefault('Accept', 'application/json')
        headers.setdefault('User-Agent', 'couchapp %s' % __version__)
        body = None
        if content is not None:
            if not isinstance(content, basestring):
                body = json.dumps(content, allow_nan=False,
                                  ensure_ascii=False).encode('utf-8')
                headers.setdefault('Content-Type', 'application/json')
            else:
                body = content
            
            headers.setdefault('Content-Length', str(len(body)))

        def _make_request(retry=1):
            try:
                return self.http.request(uri(self.uri, path, **params), method,
                                             body=body, headers=headers)
            except socket.error, e:
                if retry > 0 and e.args[0] == 54: # reset by peer
                    return _make_request(retry - 1)
                raise
        resp, data = _make_request()

        status_code = int(resp.status)
        if data and resp.get('content-type') == 'application/json':
            try:
                data = json.loads(data)
            except ValueError:
                pass

        if status_code >= 400:
            if type(data) is dict:
                error = (data.get('error'), data.get('reason'))
            else:
                error = data
            if status_code == 404:
                raise ResourceNotFound(error)
            elif status_code == 409:
                raise ResourceConflict(error)
            elif status_code == 412:
                raise PreconditionFailed(error)
            else:
                raise ServerError((status_code, error))

        return resp, data


def uri(base, *path, **query):
    """Assemble a uri based on a base, any number of path segments, and query
    string parameters.

    >>> uri('http://example.org/', '/_all_dbs')
    'http://example.org/_all_dbs'
    """
    if base and base.endswith('/'):
        base = base[:-1]
    retval = [base]

    # build the path
    path = '/'.join([''] +
                    [unicode_quote(s.strip('/')) for s in path
                     if s is not None])
    if path:
        retval.append(path)

    # build the query string
    params = []
    for name, value in query.items():
        if type(value) in (list, tuple):
            params.extend([(name, i) for i in value if i is not None])
        elif value is not None:
            if value is True:
                value = 'true'
            elif value is False:
                value = 'false'
            params.append((name, value))
    if params:
        retval.extend(['?', unicode_urlencode(params)])

    return ''.join(retval)


def unicode_quote(string, safe=''):
    if isinstance(string, unicode):
        string = string.encode('utf-8')
    return quote(string, safe)


def unicode_urlencode(data):
    if isinstance(data, dict):
        data = data.items()
    params = []
    for name, value in data:
        if isinstance(value, unicode):
            value = value.encode('utf-8')
        params.append((name, value))
    return urlencode(params)


VALID_DB_NAME = re.compile(r'^[a-z0-9_$()+-/]+$')
def validate_dbname(name):
    if not VALID_DB_NAME.match(name):
        raise ValueError('Invalid database name')
    return name
