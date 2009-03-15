#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# Copyright 2009 Benoit Chesneau <benoitc@e-engura.org>
#
# This software is licensed as described in the file LICENSE, which
# you should have received as part of this distribution.
#

import glob
import httplib
import os
import re
import shutil
import socket
import sys
import time
import urllib

# python 2.6
try:
    import json 
except ImportError:
    import simplejson as json 

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
        
try:#python 2.6, use subprocess
    from subprocess import *
    def _popen3(cmd, mode='t', bufsize=0):
        p = Popen(cmd, shell=True, bufsize=bufsize,
            stdin=PIPE, stdout=PIPE, stderr=PIPE, close_fds=True)
        return (p.stdin, p.stdout, p.stderr)
except ImportError:
    def _popen3(cmd, mode='t', bufsize=0):
        return os.popen3(cmd, mode, bufsize)
        


import httplib2
from couchdb import Server, ResourceNotFound

from couchapp.utils import _md5, to_bytestring
from couchapp.utils import *
from couchapp.utils.css_parser import CSSParser

__all__ = ['DEFAULT_SERVER_URI', 'FileManager']

DEFAULT_SERVER_URI = 'http://127.0.0.1:5984/'

COUCHAPP_VENDOR_URL = 'git://github.com/jchris/couchapp.git'
COUCHAPP_VENDOR_SCM = 'git'

external_dir = os.path.join(os.path.dirname(__file__), '_external')
VENDOR_HANDLERS = {
    'git': os.path.join(external_dir, 'git.sh')
}

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

def get_userconf():
    """ return user conf """
    
    # this should work on windows too
    homedir = os.path.expanduser('~')
    user_conffile = os.path.join(homedir, ".couchapprc")
    if os.path.isfile(user_conffile):
        try:
            return read_json(user_conffile)
        except:
            pass
    return {}
    
def _vendor_handlers():
    user_conf = get_userconf()
    vendor_handlers = VENDOR_HANDLERS
    if "vendor_handlers" in user_conf:
        try:
            vendor_handler.update(user_conf['vendor_handlers'])
        except ValueError:
            pass
    return vendor_handlers

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
        
        self.conf = get_userconf()
        
    
    @classmethod
    def generate_app(cls, app_dir):
        """Generates a CouchApp in app_dir"""
        template_paths = ['app-template', '../../app-template']
        vendor_paths = ['vendor', '../../vendor']
        
        for path in template_paths:
            template_dir = os.path.normpath(os.path.join(
                os.path.dirname(__file__), path))
            if os.path.isdir(template_dir): break
            
        for path in vendor_paths:
            vendor_dir = os.path.normpath(os.path.join(
                os.path.dirname(__file__), path))
            if os.path.isdir(vendor_dir): break
        
        try:
            shutil.copytree(template_dir, app_dir)
        except OSError, e:
            errno, message = e
            print >>sys.stderr, "Can't create a CouchApp in %s: %s" % (
                    app_dir, message)
            return
                          
        if vendor_dir:
            vendor_path = os.path.join(app_dir, 'vendor')
            try:
                shutil.copytree(vendor_dir, vendor_path)
            except OSError, e:
                errno, message = e
                print >>sys.stderr, "Can't create a CouchApp, bad vendor dir: %s" % message
                return

        cls.init(app_dir)
    
    @classmethod
    def init(cls, app_dir, db_url=''):
        """Initializes the .couchapprc, usually called after generate"""
        if not os.path.isdir(app_dir):
            print>>sys.stderr, "%s directory doesn't exist." % app_dir
            return
        
        userconf = get_userconf()
        if 'default' in userconf:
            conf = {
                "env": {
                    "default": userconf['env']['default']
                }
            }
        else:
            conf = {}
            
        rc_file = '%s/.couchapprc' % app_dir
        if not os.path.isfile(rc_file):
            if db_url:
                conf.update({ "env": { 
                    "default": {
                        "db": db_url
                    }
                }})

            write_json(rc_file, conf)
        else:
            print>>sys.stderr, "CouchApp already initialized in %s." % app_dir

    def load_metadata(self, app_dir):
        """Reads the .couchapprc to get configuration data"""
        rc_file = os.path.join(app_dir, '.couchapprc')
        conf = get_userconf()
        if os.path.isfile(rc_file):
            conf.update(read_json(rc_file, use_environment=True))
        self.conf = conf

    def push_app(self, app_dir, app_name, verbose=False, **kwargs):
        """Pushes the app specified to the CouchDB instance"""
        docid = '_design/%s' % app_name

        attach_dir = os.path.join(app_dir, '_attachments')

        manifest = []
        self.doc = doc = self.dir_to_fields(app_dir, manifest=manifest,
                verbose=verbose)

        self.objects = {}
        if 'shows' in doc:
            self.package_shows(doc['shows'], app_dir, verbose=verbose)

        if 'lists' in doc:
            self.package_shows(doc['lists'], app_dir, verbose=verbose)

        if 'validate_doc_update' in doc:
            tmp_dict = dict(validate_doc_update=doc["validate_doc_update"])
            self.package_shows(tmp_dict, app_dir, verbose=verbose)
            doc.update(tmp_dict)

        if 'views' in doc:
            self.package_views(doc["views"], app_dir, verbose=verbose)


        for db in self.db:
            if verbose >= 1:
                print "Pushing CouchApp in %s to design doc:\n%s/_design/%s" % (app_dir,
                    db.resource.uri, app_name)
                couchapp = doc.get('couchapp', False)
                if couchapp:
                  index = couchapp.get('index', False)
                else:
                  index = False
                index_url = self.make_index_url(db.resource.uri, app_name, attach_dir, index)
                if index_url:
                  print "Visit your CouchApp here:\n%s" % index_url

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

        if 'css' in new_doc['couchapp']:
            if not kwargs.get('nocss', False):
                # merge and compress css
                self.merge_css(attach_dir, doc['couchapp']['css'],
                    docid, verbose=verbose)

        if 'js' in new_doc['couchapp']:
            if not kwargs.get('nojs', False):
                # merge and compress js
                self.merge_js(attach_dir, doc['couchapp']['js'],
                    docid, verbose=verbose)

                    
        self.push_directory(attach_dir, docid, verbose=verbose)
        self.vendor_attachments(app_dir, docid, verbose=verbose)
        

    @classmethod
    def clone(cls, app_uri, app_dir, verbose=False):
        """Clone a CouchApp from app_uri into app_dir"""
        server_uri, db_name, docid = parse_uri(app_uri) 
        couchdb_server = _server(server_uri)

        try:
            db = couchdb_server.create(db_name)
        except: # db already exist
            db = couchdb_server[db_name]
 
        app_name = get_appname(docid)
        if verbose >= 1:
            print "Cloning %s to %s..." % (app_name, app_dir)
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
                if verbose >=2:
                    print "clone property: %s" % filename
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

                        if isinstance(content, basestring):
                            _ref = _md5(to_bytestring(content)).hexdigest()
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
                        if verbose >=2:
                            print "clone view not in manifest: %s" % filename
            elif key in ('shows', 'lists'):
                dir = os.path.join(app_dir, key)
                if not os.path.isdir(dir):
                    os.makedirs(dir)
                for func_name, func in design[key].iteritems():
                    filename = os.path.join(dir, '%s.js' % 
                            func_name)
                    open(filename, 'w').write(func)
                    if verbose >=2:
                        print "clone show or list not in manifest: %s" % filename
            else:
                file_dir = os.path.join(app_dir, key)
                if verbose >=2:
                    print "clone property not in manifest: %s" % key
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
                if filename.startswith('vendor'):
                    attach_parts = filename.split('/')
                    vendor_attach_dir = os.path.join(app_dir, attach_parts.pop(0),
                            attach_parts.pop(0), '_attachments')
                    file_path = os.path.join(vendor_attach_dir, '/'.join(attach_parts))
                else:
                    file_path = os.path.join(attach_dir, filename)
                current_dir = os.path.dirname(file_path)
                if not os.path.isdir(current_dir):
                    os.makedirs(current_dir)
        
                if signatures.get(filename) != sign_file(file_path):
                    content = db.get_attachment(docid, filename)
                    write_content(file_path, content)
                    if verbose>=2:
                        print "clone attachment: %s" % filename

    def dir_to_fields(self, app_dir, current_dir='', depth=0,
            manifest=[], verbose=False):
        fields={}
        if not current_dir:
            current_dir = app_dir
        for name in os.listdir(current_dir):
            current_path = os.path.join(current_dir, name)
            rel_path = current_path.split("%s/" % app_dir)[1]
            if name.startswith('.'):
                continue
            elif name.startswith('_'):
                # files starting with "_" are always "special"
                continue
            elif depth == 0 and name in ('couchapp', 'couchapp.json'):
                # we are in app_meta
                if name == "couchapp":
                    manifest.append('%s/' % rel_path)
                    content = self.dir_to_fields(app_dir, current_path,
                        depth=depth+1, manifest=manifest)
                else:
                    manifest.append(rel_path)
                    content = read_json(current_path)
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
                        depth=depth+1, manifest=manifest,
                        verbose=verbose)
            else:
                if verbose >= 2:
                    print >>sys.stderr, "push %s" % rel_path                
                content = ''
                try:
                    content = read_file(current_path)
                except UnicodeDecodeError, e:
                    print >>sys.stderr, str(e)
                if name.endswith('.json'):
                    try:
                        content = json.loads(content)
                    except ValueError:
                        if verbose >= 2:
                            print >>sys.stderr, "Json invalid in %s" % current_path
                
                # remove extension
                name, ext = os.path.splitext(name)
                if name in fields:
                    if verbose >= 2:
                        print >>sys.stderr, "%(name)s is already in properties. Can't add (%(name)s%(ext)s)" % {
                        "name": name,
                        "ext": ext
                        }
                else:
                    manifest.append(rel_path)
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
                if verbose >= 2:
                    print >>sys.stderr, "%s file not uploaded, sorry." % filename
                break
                
    def vendor_attachments(self, app_dir, docid, verbose):
        vendor_dir = os.path.join(app_dir, 'vendor')
        if not os.path.isdir(vendor_dir):
            return
            
        for name in os.listdir(vendor_dir):
            current_path = os.path.join(vendor_dir, name)
            if os.path.isdir(current_path):
                attach_dir = os.path.join(current_path, '_attachments')
                if os.path.isdir(attach_dir):
                    self.push_directory(attach_dir, docid, verbose, 
                                    vendor=name)
                    
    def push_directory(self, attach_dir, docid, verbose=False, vendor=None):
        # get attachments
        _signatures = {}
        _attachments = {}
        all_signatures = {}
        for root, dirs, files in os.walk(attach_dir):
            if files:
                for filename in files:
                    if filename.startswith('.'):
                        continue
                    else:
                        file_path = os.path.join(root, filename) 
                        name = file_path.split('%s/' % attach_dir)[1]
                        if vendor is not None:
                            name = os.path.join('vendor/%s' % vendor, name)
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
                all_signatures = metadata['signatures'].copy()
                for filename in metadata['signatures'].iterkeys():
                    if vendor is not None:
                        if filename.startswith('vendor/%s' % vendor):
                            del all_signatures[filename]
                            if filename not in _signatures:
                                db.delete_attachment(design, filename)
                            elif _signatures[filename] == metadata['signatures'][filename]:
                                del attachments[filename]
                            
                    else:
                        if not filename.startswith('vendor'):
                            del all_signatures[filename]
                            if filename not in _signatures:
                                db.delete_attachment(design, filename)
                            else:
                                if _signatures[filename] == metadata['signatures'][filename]:
                                    del attachments[filename]

            for filename, value in attachments.iteritems():
                if verbose >= 2:
                    print "Attaching %s" % filename
               
                # fix issue with httplib that raises BadStatusLine
                # error because it didn't close the connection
                self._put_attachment(db, design, value, filename)
                         
            # update signatures
            design = db[docid]
            if not 'couchapp' in design:
                design['couchapp'] = {}

            all_signatures.update(_signatures)
                
            design['couchapp'].update({'signatures': all_signatures})
            db[docid] = design

    def package_shows(self, funcs, app_dir, verbose=False):
        self.apply_lib(funcs, app_dir, verbose=verbose)
              
    def package_views(self, views, app_dir, verbose=False):
        for view, funcs in views.iteritems():
            self.apply_lib(funcs, app_dir, verbose=verbose)


    def apply_lib(self, funcs, app_dir, verbose=False):
        if not hasattr(self, "objects"):
            self.objects = {}
        for k, v in funcs.iteritems():
            if not isinstance(v, basestring):
                continue
            old_v = v
            try:
              funcs[k] = self.run_json_macros(
                              self.run_code_macros(v, app_dir, verbose=verbose), 
                              app_dir, verbose=verbose)
            except ValueError, e:
              print >>sys.stderr, "Error running !code or !json on function \"%s\": %s" % (k, e)
              sys.exit(-1)
            if old_v != funcs[k]:
                self.objects[_md5(to_bytestring(funcs[k])).hexdigest()] = old_v

    def run_code_macros(self, f_string, app_dir, verbose=False):
        def rreq(mo):
            # just read the file and return it
            path = os.path.join(app_dir, mo.group(2).strip(' '))
            library = ''
            filenum = 0
            for filename in glob.iglob(path):            
                if verbose>=2:
                    print "process code macro: %s" % filename
                try:
                    library += read_file(filename)
                except IOError, e:
                    print >>sys.stderr, e
                    sys.exit(-1)
                filenum += 1
                
            if not filenum:
                print >>sys.stderr, "Processing code: No file matching '%s'" % mo.group(2)
                sys.exit(-1)
                
            return library

        re_code = re.compile('(\/\/|#)\ ?!code (.*)')
        return re_code.sub(rreq, f_string)

    def run_json_macros(self, f_string, app_dir, verbose=False):
        included = {}
        varstrings = []

        def rjson(mo):
            if mo.group(2).startswith('_attachments'): 
                # someone  want to include from attachments
                path = os.path.join(app_dir, mo.group(2).strip(' '))
                filenum = 0
                for filename in glob.iglob(path):
                    library = ''
                    try:
                        if filename.endswith('.json'):
                            library = read_json(filename)
                        else:
                            library = read_file(filename)
                    except IOError, e:
                        print >>sys.stderr, e
                        sys.exit(1)
                    filenum += 1
                    current_file = filename.split(app_dir)[1]
                    fields = current_file.split('/')
                    count = len(fields)
                    include_to = included
                    for i, field in enumerate(fields):
                        if i+1 < count:
                            include_to[field] = {}
                            include_to = include_to[field]
                        else:
                            include_to[field] = library
                if not filenum:
                    print >>sys.stderr, "Processing code: No file matching '%s'" % mo.group(2)
                    sys.exit(-1)
            else:	
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
                    content_css = str(CSSParser(read_file(src_fpath)))
                    content_css = re_url.sub(replace_url, content_css) 
                    output_css += content_css
                    if verbose >= 2:
                        print "Merging %s in %s" % (src_fname, fname)

            if not os.path.isdir(fname_dir):
                os.makedirs(fname_dir)

            write_content(dest_path, output_css) 
            
    def make_index_url(self, uri, app_name, attach_dir, index):
        if index:
          return "%s/%s/%s/%s" % (uri, '_design', app_name, index)
        else:
          index_fpath = os.path.join(attach_dir, 'index.html')
          if os.path.isfile(index_fpath):
            return "%s/%s/%s/%s" % (uri, '_design', app_name, 'index.html')
          else:
            return False
    
    @classmethod
    def vendor_update(cls, app_dir, verbose=False):
        vendor_dir = os.path.join(app_dir, "vendor")
        if not os.path.isdir(vendor_dir):
            return

        vendor_handlers = _vendor_handlers()
 
        for name in os.listdir(vendor_dir):
            current_path = os.path.join(vendor_dir, name)
            if os.path.isdir(current_path):
                mfile = os.path.join(current_path, 'metadata.json')
                metadata = read_json(mfile)
                update_url = False
                if not metadata and name == 'couchapp':
                    update_url = COUCHAPP_VENDOR_URL
                    scm = COUCHAPP_VENDOR_SCM
                elif metadata:
                    update_url = metadata['update_url']
                    scm = metadata['scm']
                    if not scm in VENDOR_HANDLERS:
                        scm = False
                
                if update_url and scm:
                    # for now we manage only internal handlers
                    handler = vendor_handlers[scm]
                    if verbose >= 1:
                        print "Updating %s from %s" % (current_path, update_url)
                    cmd = "%s update %s %s %s" % (handler, update_url, 
                                            current_path, vendor_dir)
                    (child_stdin, child_stdout, child_stderr) = _popen3(cmd)
                    if verbose >=2:
                        print child_stdout.read()
                        err = child_stderr.read()
                        if err:
                            print >>sys.stderr, err
                        
    @classmethod
    def vendor_install(cls, app_dir, url, scm="git", verbose=False):   
        if not scm in VENDOR_HANDLERS:
            print >>sys.stderr, "%s scm isn't supported yet." % scm
            sys.exit(-1)
        vendor_dir = os.path.join(app_dir, "vendor")
        if not os.path.isdir(vendor_dir):
            os.makedirs(vendor_dir)
            
        vendor_handlers = _vendor_handlers()
        
        # get list of installed applications
        installed_apps = []
        for name in os.listdir(vendor_dir):
            current_path = os.path.join(vendor_dir, name)
            if os.path.isdir(current_path):
                installed_apps.append(name)
                
            
        handler = vendor_handlers[scm]
        cmd = "%s install %s %s" % (handler, url, vendor_dir)
        (child_stdin, child_stdout, child_stderr) = _popen3(cmd)
        err = child_stderr.read()
        if verbose >=2:
            print child_stdout.read()
        if err:
            print >>sys.stderr, err
                
        # detect new vendor application and add url so we could update later
        for name in os.listdir(vendor_dir):
            current_path = os.path.join(vendor_dir, name)
            if os.path.isdir(current_path) and name not in installed_apps:
                new_file = os.path.join(current_path, '.new')
                if os.path.isfile(new_file):
                    new_url = read_file(new_file).strip()
                    if new_url == url:
                        mfile = os.path.join(current_path, 'metadata.json')
                        write_json(mfile, {
                            "scm": scm,
                            "update_url": url
                        })
                        os.unlink(new_file)
                        return
                
    def merge_js(self, attach_dir, js_conf, docid, verbose=False):
        if "js_compressor" in self.conf:
            if not isinstance(self.conf["js_compressor"], basestring):
                print >>sys.stderr, "Warning: js_compressor settings should be a string"
                print >>sys.stderr, "         Selecting default backend (jsmin)"
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
                if os.path.isfile(src_fpath):
                    output_js += "/* %s */\n" % src_fpath
                    output_js +=  read_file(src_fpath)
                    if verbose >= 2:
                        print "merging %s in %s" % (src_fname, fname)

            if not os.path.isdir(fname_dir):
                os.makedirs(fname_dir)

            output_js = backend.compress(output_js)
            write_content(dest_path, output_js) 

