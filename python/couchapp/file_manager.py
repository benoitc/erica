#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# Copyright 2009 Benoit Chesneau <benoitc@e-engura.org>
#
# This software is licensed as described in the file COPYING, which
# you should have received as part of this distribution.
#

from glob import glob
from mimetypes import guess_type
import os
import shutil
import urlparse

# compatibility with python 2.4
try:
    from hashlib import md5 as _md5
except ImportError:
    import md5
    _md5 = md5.new

try:
    import simplejson as json
except ImportError:
    import json # Python 2.6


from couchdb import Server

class FileManager(object):
    
    def __init__(self, dburl):
        # split dburl
        parts = urlparse.urlsplit(dburl)
        if parts[0] != 'http' and parts[0] != 'https':
            raise ValueError('Invalid dburl')
        
        db_path = parts[2].strip('/').split('/')
        self.db_name = db_path[0]
        
        self.server_uri = '%s://%s' % (parts[0], parts[1])
        self.couchdb_server = Server(self.server_uri)

        # create db if it don't exist
        try:
            self.db = self.couchdb_server.create(self.db_name)
        except: # db already exist
            self.db = self.couchdb_server[self.db_name]

            

    @classmethod
    def generate_app(cls, app_dir, loud=False):
        template_dir = os.path.normpath(os.path.join(os.path.dirname(__file__),
                '../app-template'))
        shutil.copytree(template_dir, app_dir)

    def push_app(self, app_dir, app_name):
        docid = '_design/%s' % app_name

        attach_dir = os.path.join(app_dir, '_attachments')

        doc = self.dir_to_fields(app_dir)

        if docid in self.db:
            design = self.db[docid]
            design.update(doc)
            self.db[docid] = design
        else:
            self.db[docid] = doc 


        self.push_directory(attach_dir, docid)

    def _load_file(self, fname):
        f = file(fname, 'r')
        data = f.read()
        f.close
        return data

    def dir_to_fields(self, app_dir, depth=0):
        fields={}
        for name in os.listdir(app_dir):
            current_path = os.path.join(app_dir, name)
            if name.startswith('.'):
                continue
            elif depth == 0 and name.startswith('_'):
                continue
            elif os.path.isdir(current_path):
                fields[name] = self.dir_to_fields(current_path,
                        depth=depth+1)
            else:
                content = self._load_file(current_path)
                if name.endswith('.json'):
                    fields[name] = json.loads(content)
                elif name.endswith('.js'):
                    fields[name[:-3]] = content
                else:
                    fields[name] = content
        return fields
    
    def push_directory(self, attach_dir, docid):
        design = self.db[docid]

        signatures = {}
        for root, dirs, files in os.walk(attach_dir):
            if files:
                for filename in files:
                    file_path = os.path.join(root, filename)
                    file = open(file_path, 'rb')
                    name = file_path.split('%s/' % attach_dir)[1] 
                    signatures[name] = _md5(file_path).hexdigest()
                    self.db.put_attachment(design, file, name)
        
        # attachments to be removed
        if 'signatures' in design:
            for filename in design['signatures'].iterkeys():
                if filename not in signatures:
                    print filename
                    self.db.delete_attachment(design, filename)
        
        design = self.db[docid]
        design.update({'signatures': signatures})
        self.db[docid] = design
