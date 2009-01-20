#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# Copyright 2009 Benoit Chesneau <benoitc@e-engura.org>
#
# This software is licensed as described in the file LICENSE, which
# you should have received as part of this distribution.
#

from glob import glob
from mimetypes import guess_type
import os
import shutil
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
    import simplejson as json
except ImportError:
    import json # Python 2.6


from couchdb import Server, ResourceNotFound

__all__ = ['DEFAULT_SERVER_URI', 'FileManager']

DEFAULT_SERVER_URI = 'http://127.0.0.1:5984/'

def write_json(filename, content):
    f = open(filename, 'w')
    f.write(json.dumps(content))
    f.close()

def read_json(filename):
    try:
        f = file(filename, 'r')
    except IOError, e:
        if e[0] == 2:
            return {}
        raise
    data = json.loads(f.read())
    f.close()
    return data


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

def get_appname(docid):
    return docid.split('_design/')[1]


def sign_file(file_path):
    if os.path.isfile(file_path):
        f = open(file_path, 'rb')
        content = f.read()
        f.close()
        return _md5(content).hexdigest()
    return ''

class FileManager(object):
    
    def __init__(self, dbstring, app_dir='.'):
        self.app_dir = app_dir 

        # load conf
        self.load_metadata(app_dir)
       
        if not dbstring or not "/" in dbstring:
            env = self.conf.get('env', {})
            if dbstring:
                if dbstring in env:
                    db_env = env[dbstring]['db']
                else: 
                    db_env = "%s/%s" % (DEFAULT_SERVER_URI, dbstring)
            else: 
                if 'default' in env:
                    db_env = env['default']['db']
                else:
                    raise ValueError("database isn't specified")

            if isinstance(db_env, basestring):
                self.db_url = [db_env]
            else:
                self.db_url = db_env
        else:
            self.db_url = [dbstring]

        db = []
        for s in self.db_url:
            server_uri, db_name, docid = parse_uri(s)

            couchdb_server = Server(server_uri)

            # create dbs if it don't exist
            try:
                _db = couchdb_server.create(db_name)
            except: # db already exist
                _db = couchdb_server[db_name]
            db.append(_db)
        self.db = db

    @classmethod
    def generate_app(cls, app_dir):
        template_dir = os.path.normpath(os.path.join(os.path.dirname(__file__),
                '../app-template'))
        shutil.copytree(template_dir, app_dir)
        cls.init(app_dir)

    @classmethod
    def init(cls, app_dir):
        if not os.path.isdir(app_dir):
            print>>sys.stderr, "%s don't exist" % app_dir
            return
        metadata_dir = '%s/.couchapp' % app_dir
        if not os.path.isdir(metadata_dir):
            os.makedirs(metadata_dir)
            write_json('%s/rc.json' % metadata_dir, {})
        else:
            print>>sys.stderr, "couchapp already initialized"

    def load_metadata(self, app_dir):
        rc_file = os.path.join(app_dir, '.couchapp/rc.json')
        if os.path.isfile(rc_file):
            self.conf = read_json(rc_file)
            return
        self.conf = {}

    def push_app(self, app_dir, app_name, verbose=False):
        docid = '_design/%s' % app_name

        attach_dir = os.path.join(app_dir, '_attachments')

        manifest = []
        doc = self.dir_to_fields(app_dir, manifest=manifest)

        for db in self.db:
            if verbose:
                print "Pushing CouchApp in %s to %s/_design/%s" % (app_dir,
                    db.resource.uri, app_name)
            new_doc = doc.copy()

            if docid in db:
                design = db[docid]

                new_doc.update({
                    '_id': docid,
                    '_rev': design['_rev'],
                    'signatures': design.get('signatures', {}),
                    'app_meta': {
                        'manifest': manifest
                    },
                    '_attachments': design.get('_attachments', {})
                })
        
            db[docid] = new_doc 

        self.push_directory(attach_dir, docid)

    @classmethod
    def clone(cls, app_uri, app_dir):
        
        server_uri, db_name, docid = parse_uri(app_uri) 
        
        couchdb_server = Server(server_uri)
        try:
            db = couchdb_server.create(db_name)
        except: # db already exist
            db = couchdb_server[db_name]
 
        app_name = get_appname(docid)
        if not app_dir:
            app_dir = os.path.normpath(os.path.join(os.getcwd(), app_name))

        metadata_dir = '%s/.couchapp' % app_dir
        rc_file = '%s/rc.json' % metadata_dir

        if not os.path.isdir(app_dir):
            os.makedirs(app_dir)
                        
        try:
            design = db[docid]
        except ResourceNotFound:
            print >>sys.stderr, "%s don't exist" % app_name

        # init signatures
        signatures = design.get('signatures', {})

        # get manifest
        metadata = design.get('app_meta', {})
        manifest = metadata.get('manifest', {})

        if metadata:
            # save metadata in repo
            
            if manifest:
                del metadata['manifest']

            if not os.path.isdir(metadata_dir):
                os.makedirs(metadata_dir)
                write_json(rc_file, { 'app_meta': metadata }) 
            else:
                conf = read_json(rc_file)
                conf['app_meta'] = metadata
                write_json(rc_file, conf) 

        # create files from manifest
        if manifest:
            for filename in manifest:
                file_path = os.path.join(app_dir, filename)
                if filename.endswith('/'): 
                    if not os.path.isdir(file_path):
                        os.makedirs(file_path)
                else:
                    parts = filename.split('/')
                    fname = ''.join(parts[-1:])
                    v = {}
                    for key in parts[:-1]:
                        if not v:
                            v = design[key]
                        else:
                            v = v[key]
                    
                    if fname.endswith('.json'):
                        content = json.dumps(v[fname[:-5]])
                    elif fname.endswith('js'):
                        content = v[fname[:-3]]
                    else:
                        content = v[fname]

                    f = open(file_path, 'wb')
                    f.write(content)
                    f.close()
        else:
            # manifest isn't in couchapp so we try 
            # to recreate couchapp folder we save
            # others fields as json value or text if 
            #  first level
            for key in design.iterkeys():
                if key.startswith('_'): 
                    continue
                elif key == 'signatures':
                    continue
                elif key in ('show', 'views'):
                    vs_dir = os.path.join(app_dir, key)
                    if not os.path.isdir(vs_dir):
                        os.makedirs(vs_dir)
                    for vsname, vs_item in design[key].iteritems():
                        vs_item_dir = os.path.join(vs_dir, vsname)
                        if not os.path.isdir(vs_item_dir):
                            os.makedirs(vs_item_dir)
                        for func_name, func in vs_item.iteritems():
                            filename = os.path.join(vs_item_dir, '%s.js' % 
                                    func_name)
                            open(filename, 'w').write(func)
                else:
                    file_dir = os.path.join(app_dir, key)
                    if isinstance(design[key], (list, tuple,)):
                        write_json(file_dir, design[key])
                    elif isinstance(design[key], dict):
                        if not os.path.isdir(file_dir):
                            os.makedirs(file_dir)
                        for field, value in design[key].iteritems():
                            field_path = os.path.join(file_dir, field)
                            if isinstance(value, basestring):
                                open(field_path, 'w').write(value)
                            else:
                                write_json(field_path + '.json', value)
                    else:
                        open(file_dir, 'w').write(design[key])
   

        # get attachments
        if '_attachments' in design:
            attach_dir = os.path.join(app_dir, '_attachments')
            if not os.path.isdir(attach_dir):
                os.makedirs(attach_dir)
            for filename in design['_attachments'].iterkeys():
                file_path = os.path.join(attach_dir, filename)
                current_dir = os.path.dirname(file_path)
                if not os.path.isdir(current_dir):
                    os.makedirs(current_dir)
        
                if signatures.get(filename) != sign_file(file_path):
                    content = db.get_attachment(docid, filename)
                    f = open(file_path, 'wb')
                    f.write(content)
                    f.close()

    def _load_file(self, fname):
        f = file(fname, 'r')
        data = f.read()
        f.close
        return data

    def dir_to_fields(self, app_dir, current_dir='', depth=0, manifest=[]):
        fields={}
        if not current_dir:
            current_dir = app_dir
        for name in os.listdir(current_dir):
            current_path = os.path.join(current_dir, name)
            rel_path = current_path.split("%s/" % app_dir)[1]
            if name.startswith('.'):
                continue
            elif depth == 0 and name.startswith('_'):
                continue
            elif os.path.isdir(current_path):
                manifest.append('%s/' % rel_path)
                fields[name] = self.dir_to_fields(app_dir, current_path,
                        depth=depth+1, manifest=manifest)
            else:
                manifest.append(rel_path)
                content = self._load_file(current_path)
                if name.endswith('.json'):
                    fields[name[:-5]] = json.loads(content)
                elif name.endswith('.js'):
                    fields[name[:-3]] = content
                else:
                    fields[name] = content
        return fields
    
    def push_directory(self, attach_dir, docid):
        # get attachments
        _signatures = {}
        _attachments = {}
        for root, dirs, files in os.walk(attach_dir):
            if files:
                for filename in files:
                    file_path = os.path.join(root, filename)
                    
                    name = file_path.split('%s/' % attach_dir)[1] 
                    signature = sign_file(file_path)
                    _signatures[name] = signature
                    _attachments[name] = open(file_path, 'rb')
        
        # detect attachments to be removed and keep
        # only new version attachments to update.
        for db in self.db:
            design = db[docid]
            attachments = _attachments.copy()
            if 'signatures' in design:
                for filename in design['signatures'].iterkeys():
                    if filename not in _signatures:
                        db.delete_attachment(design, filename)
                    else:
                        if _signatures[filename] == design['signatures'][filename]:
                            del attachments[filename]

            for filename, value in attachments.iteritems():
                db.put_attachment(design, value, filename)
       
            # update signatures
            design = db[docid]
            design['signatures'] = _signatures
            db[docid] = design
