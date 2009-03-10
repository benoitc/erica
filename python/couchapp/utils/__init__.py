#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# Copyright 2009 Benoit Chesneau <benoitc@e-engura.org>
#
# This software is licensed as described in the file LICENSE, which
# you should have received as part of this distribution.
#

import codecs
import os
import string
import sys
import urlparse
import urllib

# compatibility with python 2.4
try:
    from hashlib import md5 as _md5
except ImportError:
    import md5
    _md5 = md5.new

try:
    import json 
except ImportError:
    import simplejson as json 


def in_couchapp():
    current_path = os.getcwd()
    old_dirs = []
    while 1:
        dirs = os.listdir(current_path)
        if dirs == old_dirs: 
            return False
        if '.couchapprc' in dirs: break
        current_path = os.path.normpath(os.path.join(current_path, '../'))
        old_dirs = dirs
    return current_path

def parse_uri(string):
    parts = urlparse.urlsplit(urllib.unquote(string))
    if parts[0] != 'http' and parts[0] != 'https':
        raise ValueError('Invalid dbstring')
     
    path = parts[2].strip('/').split('/')

    dbname = ''
    docid = ''
    if len(path) >= 1:
        db_parts=[]
        i = 0
        while 1:
            try:
                p = path[i]
            except IndexError:
                break

            if p == '_design': break
            db_parts.append(p)
            i = i + 1
        dbname = '/'.join(db_parts)
        
        if i < len(path) - 1:
            docid = '/'.join(path[i:])

    server_uri = '%s://%s' % (parts[0], parts[1])
    return server_uri, dbname, docid


def parse_auth(string):
    parts = urlparse.urlsplit(urllib.unquote(string))
    
    server_parts = parts[1].split('@')
    if ":" in server_parts[0]:
        username, password = server_parts[0].split(":")
    else:
        username = server_parts[0]
        password = ''

    server_uri = "%s://%s" % (parts[0], server_parts[1])

    return username, password, server_uri

def get_appname(docid):
    return docid.split('_design/')[1]


def to_bytestring(s):
    if not isinstance(s, basestring):
        return s
    if isinstance(s, unicode):
        return s.encode('utf-8')
    else:
        return s

def read_file(fname):
    f = codecs.open(fname, 'rb', "utf-8")
    data = f.read()
    f.close()
    return data

def sign_file(file_path):
    if os.path.isfile(file_path):
        f = open(file_path, 'rb')
        content = f.read()
        f.close()
        return _md5(content).hexdigest()
    return ''

def write_content(fname, content):
    f = open(fname, 'wb')
    f.write(to_bytestring(content))
    f.close()

def write_json(filename, content):
    write_content(filename, json.dumps(content))

def read_json(filename, use_environment=False):
    try:
        data = read_file(filename)
    except IOError, e:
        if e[0] == 2:
            return {}
        raise

    if use_environment:
        data = string.Template(data).substitute(os.environ)

    try:
        data = json.loads(data)
    except ValueError:
        print >>sys.stderr, "Json is invalid, can't load %s" % filename
        return {}
    return data
