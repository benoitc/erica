#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# Copyright 2009 Benoit Chesneau <benoitc@e-engura.org>
#
# This software is licensed as described in the file LICENSE, which
# you should have received as part of this distribution.
#

import httplib
import os
import re
import shutil
import socket
import sys
import time
import urllib

try:
    import simplejson as json
except ImportError:
    import json # Python 2.6


# backport os.path.relpath if python < 2.6
try:
    import os.path.relpath as _relpath
except ImportError:
    def _relpath(path, start=os.curdir):
        if not path:
            raise ValueError("no path specified")
        
        start_list = os.path.abspath(start).split("/")
        path_list = os.path.abspath(path).split("/")

        # Work out how much of the filepath is shared by start and path.
        i = len(os.path.commonprefix([start_list, path_list]))

        rel_list = ['..'] * (len(start_list)-i) + path_list[i:]
        if not rel_list:
            return os.curdir
        return os.path.join(*rel_list)


import httplib2
from couchdb import Server, ResourceNotFound

from couchapp.utils import _md5
from couchapp.utils import *
from couchapp.utils.css_parser import CSSParser

__all__ = ['DEFAULT_SERVER_URI', 'FileManager']

DEFAULT_SERVER_URI = 'http://127.0.0.1:5984/'

def _server(server_uri):
    if "@" in server_uri:
        http = httplib2.Http()
        username, password, server_uri = parse_auth(server_uri) 
        couchdb_server = Server(server_uri)
        http.add_credentials(username, password)
        couchdb_server.resource.http = http
    else:
        couchdb_server = Server(server_uri)
    return couchdb_server

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
 
            couchdb_server = _server(server_uri)

            # create dbs if it don't exist
            try:
                _db = couchdb_server.create(db_name)
            except: # db already exist
                _db = couchdb_server[db_name]
            db.append(_db)
        self.db = db

    @classmethod
    def generate_app(cls, app_dir):
        paths = ['app-template', '../../app-template']
        
        for path in paths:
            template_dir = os.path.normpath(os.path.join(
                os.path.dirname(__file__), path))
            if os.path.isdir(template_dir): break
        
        try:
            shutil.copytree(template_dir, app_dir)
        except OSError, e:
            errno, message = e
            print >>sys.stderr, "Cant' create couchapp in %s: %s" % (
                    app_dir, message)
            return

        cls.init(app_dir)

    @classmethod
    def init(cls, app_dir, db_url=''):
        if not os.path.isdir(app_dir):
            print>>sys.stderr, "%s don't exist" % app_dir
            return
        rc_file = '%s/.couchapprc' % app_dir
        if not os.path.isfile(rc_file):
            conf = {}
            if db_url:
                conf.update({ "env": { 
                    "default": {
                        "db": db_url
                    }
                }})

            write_json(rc_file, conf)
        else:
            print>>sys.stderr, "couchapp already initialized"

    def load_metadata(self, app_dir):
        rc_file = os.path.join(app_dir, '.couchapprc')
        if os.path.isfile(rc_file):
            self.conf = read_json(rc_file)
            return
        self.conf = {}

    def push_app(self, app_dir, app_name, verbose=False):
        docid = '_design/%s' % app_name

        attach_dir = os.path.join(app_dir, '_attachments')

        manifest = []
        self.doc = doc = self.dir_to_fields(app_dir, manifest=manifest)

        self.objects = {}
        if 'shows' in doc:
            self.package_shows(doc['shows'])

        if 'lists' in doc:
            self.package_shows(doc['lists'])

        if 'views' in doc:
            self.package_views(doc["views"])


        for db in self.db:
            if verbose:
                print "Pushing CouchApp in %s to %s/_design/%s" % (app_dir,
                    db.resource.uri, app_name)
            new_doc = doc.copy()

            if docid in db:
                design = db[docid]
                _app_meta = design.get('couchapp', {})

                app_meta = {
                    'manifest': manifest,
                    'signatures': _app_meta.get('signatures', {}),
                    'objects': self.objects
                }

                new_doc.update({
                    '_id': docid,
                    '_rev': design['_rev'],
                    'couchapp': app_meta,
                    '_attachments': design.get('_attachments', {})
                })
            else:
                new_doc.update({
                    'couchapp': {
                        'manifest': manifest,
                        'objects': self.objects
                    }
                })

            if 'couchapp' in doc:
                new_doc['couchapp'].update(doc['couchapp'])

            db[docid] = new_doc 

        if 'css' in doc['couchapp']:
            # merge and compress css
            self.merge_css(attach_dir, doc['couchapp']['css'],
                    docid, verbose=verbose)

        if 'js' in doc['couchapp']:
            # merge and compress js
            self.merge_js(attach_dir, doc['couchapp']['js'],
                    docid, verbose=verbose)

        self.push_directory(attach_dir, docid, verbose=verbose)

        

    @classmethod
    def clone(cls, app_uri, app_dir, verbose=False):
        server_uri, db_name, docid = parse_uri(app_uri) 
        couchdb_server = _server(server_uri)

        try:
            db = couchdb_server.create(db_name)
        except: # db already exist
            db = couchdb_server[db_name]
 
        app_name = get_appname(docid)
        if verbose:
            print "Clone %s" % app_name
        if not app_dir:
            app_dir = os.path.normpath(os.path.join(os.getcwd(), app_name))

        rc_file = os.path.join(app_dir, '.couchapprc')

        if not os.path.isdir(app_dir):
            os.makedirs(app_dir)
        else:
            # delete only if there is .couchapp folder
            if os.path.isfile(rc_file):
                for root, dirs, files in os.walk(app_dir,
                        topdown=False):
                    if root == app_dir:
                        if '_attachments' in dirs:
                            dirs.remove('_attachments') 
                        if '.couchapprc' in files:
                            files.remove('.couchapprc')
                    for name in files:
                        os.remove(os.path.join(root, name))

                    for name in dirs:
                        os.rmdir(os.path.join(root, name))
        
        try:
            design = db[docid]
        except ResourceNotFound:
            print >>sys.stderr, "%s don't exist" % app_name
            return

        metadata = design.get('couchapp', {})
        
        # get manifest
        manifest = metadata.get('manifest', {})

        # get signatures
        signatures = metadata.get('signatures', {})

        # get objects refs
        objects = metadata.get('objects', {})

        conf = read_json(rc_file)
        if not 'env' in conf:
            conf['env'] = {}
        conf['env'].update({
            'origin': {
                'db': db.resource.uri
            }
        })

        write_json(rc_file, conf) 

        # create files from manifest
        if manifest:
            for filename in manifest:
                file_path = os.path.join(app_dir, filename)
                if filename.endswith('/'): 
                    if not os.path.isdir(file_path):
                        os.makedirs(file_path)
                elif filename == "couchapp.json":
                    continue
                else:
                    parts = filename.split('/')
                    fname = parts.pop()
                    v = design
                    while 1:
                        try:
                            for key in parts:
                                v = v[key]
                        except KeyError:
                            break

                        # remove extension
                        last_key, ext = os.path.splitext(fname)

                        # make sure key exist
                        try:
                            content = v[last_key]
                        except KeyError:
                            break

                        _ref = _md5(content).hexdigest()
                        if objects and _ref in objects:
                            content = objects[_ref]

                        if fname.endswith('.json'):
                                content = json.dumps(content)

                        del v[last_key]

                        # make sure file dir have been created
                        file_dir = os.path.dirname(file_path)
                        if not os.path.isdir(file_dir):
                            os.makedirs(file_dir)
                        
                        write_content(file_path, content)

                        # remove the key from design doc
                        temp = design
                        for key2 in parts:
                            if key2 == key:
                                if not temp[key2]:
                                    del temp[key2]
                                break
                            temp = temp[key2]
        
        # second pass for missing key or in case
        # manifest isn't in app
        for key in design.iterkeys():
            if key.startswith('_'): 
                continue
            elif key in ('couchapp'):
                app_meta = design['couchapp'].copy()
                if 'signatures' in app_meta:
                    del app_meta['signatures']
                if 'manifest' in app_meta:
                    del app_meta['manifest']
                if 'objects' in app_meta:
                    del app_meta['objects']
                if app_meta:
                    couchapp_file = os.path.join(app_dir, 'couchapp.json')
                    write_json(couchapp_file, app_meta)
            elif key in ('views'):
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
            elif key in ('shows', 'lists'):
                dir = os.path.join(app_dir, key)
                if not os.path.isdir(dir):
                    os.makedirs(dir)
                for func_name, func in design[key].iteritems():
                    filename = os.path.join(dir, '%s.js' % 
                            func_name)
                    open(filename, 'w').write(func)
            else:
                file_dir = os.path.join(app_dir, key)
                if isinstance(design[key], (list, tuple,)):
                    write_json(file_dir + ".json", design[key])
                elif isinstance(design[key], dict):
                    if not os.path.isdir(file_dir):
                        os.makedirs(file_dir)
                    for field, value in design[key].iteritems():
                        field_path = os.path.join(file_dir, field)
                        if isinstance(value, basestring):
                            write_content(field_path, value)
                        else:
                            write_json(field_path + '.json', value)
                else:
                    value = design[key]
                    if not isinstance(value, basestring):
                        value = str(value)
                    write_content(file_dir, value)
   

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
                    write_content(file_path, content)

    def _load_file(self, fname):
        f = file(fname, 'rb')
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
            elif depth == 0 and name in ('couchapp', 'couchapp.json'):
                # we are in app_meta
                if name == "couchapp":
                    manifest.append('%s/' % rel_path)
                    content = self.dir_to_fields(app_dir, current_path,
                        depth=depth+1, manifest=manifest)
                else:
                    manifest.append(rel_path)
                    content = self._load_file(current_path)
                    content = json.loads(content)
                    if not isinstance(content, dict):
                        content = { "meta": content }
                if 'signatures' in content:
                    del content['signatures']

                if 'manifest' in content:
                    del content['manifest']

                if 'objects' in content:
                    del content['objects']

                if 'couchapp' in fields:
                    fields['couchapp'].update(content)
                else:
                    fields['couchapp'] = content
            elif os.path.isdir(current_path):
                manifest.append('%s/' % rel_path)
                fields[name] = self.dir_to_fields(app_dir, current_path,
                        depth=depth+1, manifest=manifest)
            else:
                manifest.append(rel_path)
                content = self._load_file(current_path)
                if name.endswith('.json'):
                    content = json.loads(content)
                
                # remove extension
                name, ext = os.path.splitext(name) 
                fields[name] = content
        return fields
    
    def _put_attachment(self, db, doc, content, filename):
        nb_try = 0
        while True:
            error = False
            try:
                db.put_attachment(doc, content, filename)
            except (socket.error, httplib.BadStatusLine):
                time.sleep(0.4)
                error = True

            nb_try = nb_try +1
            if not error:
                break

            if nb_try > 3:
                print >>sys.stderr, "%s not uploaded" % filename
                break

    def push_directory(self, attach_dir, docid, verbose):
        # get attachments
        _signatures = {}
        _attachments = {}
        for root, dirs, files in os.walk(attach_dir):
            if files:
                for filename in files:
                    if filename.startswith('.'):
                        continue
                    else:
                        file_path = os.path.join(root, filename) 
                        name = file_path.split('%s/' % attach_dir)[1] 
                        signature = sign_file(file_path)
                        _signatures[name] = signature
                        _attachments[name] = open(file_path, 'rb')
        
        # detect attachments to be removed and keep
        # only new version attachments to update.
        for db in self.db:
            design = db[docid]
            metadata = design.get('couchapp', {})
            attachments = _attachments.copy()
            if 'signatures' in metadata:
                for filename in metadata['signatures'].iterkeys():
                    if filename not in _signatures:
                        db.delete_attachment(design, filename)
                    else:
                        if _signatures[filename] == metadata['signatures'][filename]:
                            del attachments[filename]

            for filename, value in attachments.iteritems():
                if verbose:
                    print "Attach %s" % filename
               
                # fix issue with httplib that raises BadStatusLine
                # error because it didn't close the connection
                self._put_attachment(db, design, value, filename)
                         
            # update signatures
            design = db[docid]
            if not 'couchapp' in design:
                design['couchapp'] = {}
            design['couchapp'].update({'signatures': _signatures})
            db[docid] = design

    def package_shows(self, funcs):
        self.apply_lib(funcs)

    def package_views(self, views):
        for view, funcs in views.iteritems():
            self.apply_lib(funcs)

    def apply_lib(self, funcs):
        if not hasattr(self, "objects"):
            self.objects = {}
        for k, v in funcs.iteritems():
            if not isinstance(v, basestring):
                continue
            old_v = v
            funcs[k] = self.process_include(self.process_requires(v))
            if old_v != funcs[k]:
                self.objects[_md5(funcs[k]).hexdigest()] = old_v

    def process_requires(self, f_string):
        def rreq(mo):
            fields = mo.group(2).split('.')
            library = self.doc
            for field in fields:
                if not field in library: break
                library = library[field]
            return library

        re_code = re.compile('(\/\/|#)\ ?!code (.*)')
        return re_code.sub(rreq, f_string)

    def process_include(self, f_string):
        included = {}
        varstrings = []

        def rjson(mo):
            fields = mo.group(2).split('.')
            library = self.doc
            count = len(fields)
            include_to = included
            for i, field in enumerate(fields):
                if not field in library: break
                library = library[field]
                if i+1 < count:
                    include_to[field] = include_to.get(field, {})
                    include_to = include_to[field]
                else:
                    include_to[field] = library

            return f_string

        def rjson2(mo):
            return '\n'.join(varstrings)

        re_json = re.compile('(\/\/|#)\ ?!json (.*)')
        re_json.sub(rjson, f_string)

        if not included:
            return f_string

        for k, v in included.iteritems():
            varstrings.append("var %s = %s;" % (k, json.dumps(v)))

        return re_json.sub(rjson2, f_string)

    def merge_css(self, attach_dir, css_conf, docid, verbose=False):
        re_url = re.compile('url\s*\(([^\s"].*)\)')

        src_fpath = ''
        fname_dir = ''

        def replace_url(mo):
            """ make sure urls are relative to css path """
            css_url = mo.group(0)[4:].strip(")").replace("'", "").replace('"','')
            css_path = os.path.join(os.path.dirname(src_fpath),
                    css_url)

            rel_path = _relpath(css_path, fname_dir)
            return "url(%s)" % rel_path
        
        for fname, src_files in css_conf.iteritems():
            output_css = ''

            dest_path = os.path.join(attach_dir, fname)
            fname_dir = os.path.dirname(dest_path)

            for src_fname in src_files:
                src_fpath = os.path.join(attach_dir, src_fname)
                
                if os.path.exists(src_fpath):
                    content_css = str(CSSParser(self._load_file(src_fpath)))
                    content_css = re_url.sub(replace_url, content_css) 
                    output_css += content_css
                    if verbose:
                        print "merge %s in %s" % (src_fname, fname)

            if not os.path.isdir(fname_dir):
                os.makedirs(fname_dir)

            write_content(dest_path, output_css) 
            
    def merge_js(self, attach_dir, js_conf, docid, verbose=False):
        if "js_compressor" in self.conf:
            if not isinstance(self.conf["js_compressor"], basestring):
                print >>sys.stderr, "js_compressor settings should be a string"
                print >>sys.stderr, "back to default backend"
                import couchapp.utils.jsmin as backend
            else:
                try:
                    backend = __import__(self.conf['js_compressor'], {}, {}, [''])
                except ImportError:
                    import couchapp.utils.jsmin as backend
        else:
            import couchapp.utils.jsmin as backend

        if verbose:
            backend.about()

        for fname, src_files in js_conf.iteritems():
            output_js = ''

            dest_path = os.path.join(attach_dir, fname)
            fname_dir = os.path.dirname(dest_path)

            for src_fname in src_files:
                src_fpath = os.path.join(attach_dir, src_fname)
                
                if os.path.exists(src_fpath):
                    output_js += "/* %s */\n" % src_fpath
                    output_js +=  self._load_file(src_fpath)
                    if verbose:
                        print "merge %s in %s" % (src_fname, fname)

            if not os.path.isdir(fname_dir):
                os.makedirs(fname_dir)

            output_js = backend.compress(output_js)
            write_content(dest_path, output_js) 

