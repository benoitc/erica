# -*- coding: utf-8 -*-
#
# Copyright 2009 Benoit Chesneau <benoitc@e-engura.org>
#
# This software is licensed as described in the file LICENSE, which
# you should have received as part of this distribution.
#

import base64
import copy
from hashlib import md5
import mimetypes
import os
import re
import shutil
import sys

from couchapp.contrib import couchdb
from couchapp.contrib import simplejson as json
from couchapp.errors import *
from couchapp.extensions import Extensions
from couchapp.macros import package_views, package_shows
from couchapp.utils import *

class CouchApp(object):
    """ app object. used to push/clone/init/create a couchapp """
    
    default_locations = (
        ("template", ['app', '../../templates/app']),
        ("vendor", ['vendor', '../../vendor'])
    )
        
    def __init__(self, app_dir, ui):
        """
        Constructor for Couchapp object.
        
        :params ui: ui instance
        
        """
        self.app_dir = ui.realpath(app_dir)
        self.ui = ui
        self.ui.updateconfig(app_dir)
        self.app_dir = app_dir
         # load extensions
        self.extensions = Extensions(self)
        self.extensions.load()
        
    def initialize(self, db_url=None):
        """
        Initialize an app. It basically create the .couchapprc file
        and add default conf.
        
        :attr default_conf: dict, default configuration
        :attr verbose: boolean, default False
        :return: boolean, dict. { 'ok': True } if ok, { 'ok': False, 'error': message } 
        if something was wrong?
        """
         
        if not self.ui.isdir(self.app_dir) and self.ui.verbose:
            self.ui.logger.error("%s directory doesn't exist." % self.app_dir)
            
        # set default_conf
        default_conf = {}
        if db_url:
            default_conf = { "env": { "default": { "db": db_url } } }

        rc_file = self.ui.rjoin(self.app_dir, '.couchapprc')
        if not self.ui.isfile(rc_file):
            self.ui.write_json(rc_file, default_conf)
        elif self.ui.verbose:
            raise AppError("CouchApp already initialized in %s." % self.app_dir)


    def generate(self, kind='app', name=None, template=None):
        if kind not in ["app", "view", "list", "show", 'filter', 'function', 'vendor']:
            raise AppError("Can't generate %s in your couchapp" % kind)
        
        if kind == "app":
            self.generate_app(template=template)
        else:
            if name is None:
                raise AppError("Can't generate %s function, name is missing" % kind)
            self.generate_function(kind, name, template)
        
        
    def generate_app(self, template=None):
        """ Generates a CouchApp in app_dir 
        
        :attr verbose: boolean, default False
        :return: boolean, dict. { 'ok': True } if ok, { 'ok': False, 'error': message } 
        if something was wrong.
        """
        DEFAULT_APP_TREE = [
            '_attachments',
            'lists',
            'shows',
            'updates',
            'views'
        ]
        
        TEMPLATES = ['app', 'vendor']
        prefix = ''
        if template is not None:
            prefix = self.ui.rjoin(*template.split('/'))
        try:
            os.makedirs(self.app_dir)
        except OSError, e:
            errno, message = e
            raise AppError("Can't create a CouchApp in %s: %s" % (
                    self.app_dir, message))
        
        for n in DEFAULT_APP_TREE:
            path = self.ui.rjoin(self.app_dir, n)
            self.ui.makedirs(path)
        
        for t in TEMPLATES:
            app_dir = self.app_dir
            if prefix:
                # we do the job twice for now to make sure an app or vendor
                # template exist in user template location
                # fast on linux since there is only one user dir location
                # but could be a little slower on windows
                for user_location in user_path():
                    location = os.path.join(user_location, 'templates', prefix, t)
                    if self.ui.exists(location):
                        if t == "vendor":
                            app_dir = self.ui.rjoin(app_dir, "vendor")
                            try:
                                os.makedirs(app_dir)
                            except:
                                pass
                        t = self.ui.rjoin(prefix, t)
                        break
                
            self.ui.copy_helper(app_dir, t)

        self.initialize()
        self.extensions.notify("post-generate", self.ui, self)
        
    def generate_function(self, kind, name, template=None):
        functions_path = ['functions']
        if template is not None:
            functions_path = []
            rel_path = self.ui.rjoin(*template.split('/'))
            template_dir =  self.ui.find_template_dir(rel_path)
        else:
            template_dir = self.ui.find_template_dir()
        if template_dir:
            functions = []
            path = self.app_dir
            if kind == "view":
                path = self.ui.rjoin(path, "%ss" % kind, name)
                if self.ui.exists(path):
                    raise AppError("The view %s already exists" % name)
                functions = [('map.js', 'map.js'), ('reduce.js', 'reduce.js')]
            elif kind == "function":
                functions = [('%s.js' % name, '%s.js' % name)]
            elif kind == "vendor":
                app_dir = self.ui.rjoin(self.app_dir, "vendor", name)
                try:
                    os.makedirs(app_dir)
                except:
                    pass
                target_path = self.ui.rjoin(*template.split('/'))
                self.ui.copy_helper(app_dir, target_path)
                return
            else:
                path = self.ui.rjoin(path, "%ss" % kind)
                functions = [('%s.js' % kind, "%s.js" % name )]
            try:
                os.makedirs(path)
            except:
                pass
            
            for template, target in functions:
                target_path = self.ui.rjoin(path, target)
                root_path = [template_dir] + functions_path + [template]
                root = self.ui.rjoin(*root_path)
                try:
                    shutil.copy2(root, target_path)
                except:
                    self.ui.logger.info("%s not found in %s" % (template, self.ui.rjoin(*root_path[:-1])))
        else:
            raise AppError("Defaults templates not found. Check your install.")
        
    def clone(self, app_uri):
        """Clone a CouchApp from app_uri into app_dir"""
       
        server_uri, db_name, docid = parse_uri(app_uri) 
        app_name = get_appname(docid)
        db = self.ui.db(server_uri, db_name)
            
        try:
            design_doc = db[docid]
        except ResourceNotFound:
            raise RequestError('cant get couchapp "%s"' % app_name)

        app_name = get_appname(design_doc['_id'])
        if not self.app_dir or self.app_dir == ".":
            self.app_dir = self.ui.rjoin(self.app_dir, app_name)
            
        self.extensions.notify("pre-clone", self.ui, self, db=db, design_doc=design_doc)
        
        
        rc_file = self.ui.rjoin(self.app_dir, '.couchapprc')

        if not self.ui.isdir(self.app_dir):
            self.ui.makedirs(self.app_dir)
        elif self.ui.isfile(rc_file):
            raise AppError("an app already exist here: %s" % self.app_dir)  
        
        if self.ui.verbose >= 1:
            self.ui.logger.info("Cloning %s to %s..." % (app_name, self.app_dir))
            
        # clone
        self.designdoc_to_fs(db, design_doc)
            
        conf = {}
        conf['env'] = {
            'origin': {
                'db': db.resource.uri
            }
        }
        self.ui.write_json(rc_file, conf)
        self.extensions.notify("post-clone", self.ui, self, db=db, design_doc=design_doc)
        
        
    def push(self, dbstring, app_name, **kwargs):
        """Pushes the app specified to the CouchDB instance
        
        :attr dbstring: string, db url or db environment name.
        :attr app_name: name of app to push. 
        :attr verbose: boolean, default is False
        """
        self.extensions.notify("pre-push", self.ui, self)
        app_name = self.ui.get_app_name(dbstring, app_name)
        design_doc = self.fs_to_designdoc(app_name)
        
        docid = design_doc['_id']
        attach_dir = self.ui.rjoin(self.app_dir, '_attachments')
        new_doc = copy.deepcopy(design_doc)
        
        couchapp = design_doc.get('couchapp', {})
        if couchapp:
            index = couchapp.get('index', False)
        else:
            index = False
            new_doc['couchapp'] = {}

        # get docs from _docs folder
        docs_dir = self.ui.rjoin(self.app_dir, '_docs')
        if self.ui.isdir(docs_dir):
            docs = self.fs_to_docs(docs_dir)
        else:
            docs = []
        
        # do we export ?
        if kwargs.get('export', False):
            # process attachments
            design = copy.deepcopy(design_doc)
            del new_doc['_attachments']
            if not 'couchapp' in design:
                design['couchapp'] = {}
                attachments = design['_attachments']
                _length = design['couchapp'].get('length', {})
                
                new_attachments = {}
                for filename, value in attachments.iteritems():
                    content_length = _length.get(filename, None)
                    if self.ui.verbose >= 2:
                        self.ui.logger.info("Attaching %s (%s)" % (filename, content_length))

                    f = open(value, "rb")
                    # fix issue with httplib that raises BadStatusLine
                    # error because it didn't close the connection
                    new_attachments[filename] = {
                        "content_type": ';'.join(filter(None, mimetypes.guess_type(filename))),
                        "data": base64.b64encode(f.read()),
                    }

                # update signatures
                if not 'couchapp' in new_doc:
                    new_doc['couchapp'] = {}
                new_doc['_attachments'] = new_attachments
            
            
            if kwargs.get('output', None) is not None:
                self.ui.write_json(kwargs.get('output'), new_doc)
            else:
                print json.dumps(new_doc)    
            self.extensions.notify("post-push", self.ui, self, db=None)
            
            return
      
        # we process attachments later
        del new_doc['_attachments']
        if 'signatures' in new_doc['couchapp']:
            del new_doc['couchapp']['signatures']
            
        for key, value in self.ui.conf.items():
            if key == "env":
                continue
            elif key == "length":
                continue
            elif key == "manifest":
                continue
            elif key == "objects":
                continue
            elif key == "signatures":
                continue
            elif key not in new_doc['couchapp']:
                new_doc['couchapp'][key] = value

        for db in self.ui.get_db(dbstring):
            if self.ui.verbose >= 1:
                self.ui.logger.info("Pushing CouchApp in %s to design doc:\n%s/%s" % (self.app_dir,
                    db.resource.uri, docid))
            
            index_url = self.index_url(db.resource.uri, app_name, attach_dir, index)
            

            if docid in db:
                design = db[docid]
                _app_meta = design.get('couchapp', {})

                new_doc['couchapp'] ['signatures'] = _app_meta.get('signatures', {})

                new_doc.update({
                    '_rev': design['_rev'],
                    '_attachments': design.get('_attachments', {})
                })
            else:
                 new_doc.update({'_attachments': {}})
                
            if not kwargs.get('atomic', False):
                db[docid] = new_doc
                self.send_attachments(db, design_doc)
            else:
                self.encode_attachments(db, design_doc, new_doc)
                db[docid] = new_doc
                
            # send docs maybe we should do bullk update here
            for doc in docs:
                new_doc = copy.deepcopy(doc)
                inline_attachments = {}
                if isinstance(doc.get('_attachments', False), dict):
                    first_attachment = doc['_attachments'].values()[0]
                    if isinstance(first_attachment, dict):
                        inline_attachments = doc['_attachments']
                
                docid = new_doc['_id']
                if docid in db:
                    old_doc = db[docid]
                    doc_meta = old_doc.get('couchapp', {})
                    doc['couchapp']['signatures'] = doc_meta.get('signatures', {})
                    new_doc.update({
                        '_rev': old_doc['_rev'],
                        '_attachments': old_doc.get('_attachments', inline_attachments)
                    })
                else:
                    new_doc['couchapp']['signatures'] = {}
                    new_doc.update({'_attachments': inline_attachments})
                     
                if not kwargs.get('atomic', False):
                    db[docid] = new_doc
                    if not inline_attachments:
                        self.send_attachments(db, doc)
                else:
                    if not inline_attachments:
                        self.encode_attachments(db, doc, new_doc)
                    db[docid] = new_doc
                
            self.extensions.notify("post-push", self.ui, self, db=db)
            if index_url:
                self.ui.logger.info("Visit your CouchApp here:\n%s" % index_url)
                 
    def push_docs(self, dbstring, docs_dir, **kwargs):
        docs_dir = self.ui.realpath(docs_dir)
        docs = self.fs_to_docs(docs_dir)
        for db in self.ui.get_db(dbstring):
            # send docs maybe we should do bullk update here
            for doc in docs:
                new_doc = copy.deepcopy(doc)
                docid = new_doc['_id']
                if docid in db:
                    old_doc = db[docid]
                    doc_meta = old_doc.get('couchapp', {})
                    doc['couchapp']['signatures'] = doc_meta.get('signatures', {})
                    new_doc.update({
                        '_rev': old_doc['_rev'],
                        '_attachments': old_doc.get('_attachments', {})
                    })
                else:
                    new_doc['couchapp']['signatures'] = {}
                    new_doc.update({'_attachments': {}})
                    
                if not kwargs.get('atomic', False):
                    db[docid] = new_doc
                    self.send_attachments(db, doc)
                else:
                    self.encode_attachments(db, doc, new_doc)
                    db[docid] = new_doc

    def send_attachments(self, db, design_doc):
        # init vars
        all_signatures = {}                  
        if not 'couchapp' in design_doc:
            design['couchapp'] = {}
        
        _signatures = design_doc['couchapp'].get('signatures', {})
        _length = design_doc['couchapp'].get('length', {})
        _attachments = design_doc.get('_attachments', {})
        docid = design_doc['_id']
        
        # detect attachments to be removed and keep
        # only new version attachments to update.
        design = db[docid]
        metadata = design.get('couchapp', {})
        attachments = _attachments.copy()
        if 'signatures' in metadata:
            all_signatures = metadata['signatures'].copy()
            for filename in metadata['signatures'].iterkeys():
                if filename not in _signatures:
                    db.delete_attachment(design, filename)
                    del all_signatures[filename]
                elif _signatures[filename] == metadata['signatures'][filename]:
                    del attachments[filename]
        for filename, value in attachments.iteritems():
            content_length = _length.get(filename, None)
            if self.ui.verbose >= 2:
                self.ui.logger.info("Attaching %s (%s)" % (filename, content_length))
            
            if isinstance(value, basestring):
                f = open(value, "rb")
                # fix issue with httplib that raises BadStatusLine
                # error because it didn't close the connection
                self.ui.put_attachment(db, design, f, filename,
                        content_length=content_length)
                     
        # update signatures
        design = db[docid]
        if not 'couchapp' in design:
            design['couchapp'] = {}

        all_signatures.update(_signatures)
        design['couchapp'].update({'signatures': _signatures})
        db[docid] = design
        
    def encode_attachments(self, db, design_doc, new_doc):
        all_signatures = {}                  
        if not 'couchapp' in design_doc:
            design['couchapp'] = {}
        _signatures = design_doc['couchapp'].get('signatures', {})
        _length = design_doc['couchapp'].get('length', {})
        _attachments = design_doc.get('_attachments', {})
        docid = design_doc['_id']
        
        attachments = _attachments.copy()
        design = {}
        try:
            design = db[docid]
        except couchdb.ResourceNotFound:
            pass
            
        new_attachments = design.get('_attachments', {})
        metadata = design.get('couchapp', {})
        if 'signatures' in metadata:
            all_signatures = metadata['signatures'].copy()
            for filename in metadata['signatures'].iterkeys():
                if not filename in _signatures:
                    del new_attachments[filename]
                    del all_signatures[filename]
                elif _signatures[filename] == metadata['signatures'][filename]:
                    del attachments[filename]
        
        for filename, value in attachments.iteritems():
            content_length = _length.get(filename, None)
            if self.ui.verbose >= 2:
                self.ui.logger.info("Attaching %s (%s)" % (filename, content_length))
            
            if isinstance(value, basestring):
                f = open(value, "rb")
                # fix issue with httplib that raises BadStatusLine
                # error because it didn't close the connection
                new_attachments[filename] = {
                    "content_type": ';'.join(filter(None, mimetypes.guess_type(filename))),
                    "data": base64.b64encode(f.read()),
                    }
                     
        # update signatures
        if not 'couchapp' in new_doc:
            new_doc['couchapp'] = {}

        all_signatures.update(_signatures)
        new_doc['couchapp'].update({'signatures': _signatures})
        new_doc['_attachments'] = new_attachments
        
    def fs_to_docs(self, docs_dir):
        """
        function to add docs from docs_dir. docs could be json.file or folders.
        :attr docs_dir: doc dir name
        """
        docs = []
        for name in os.listdir(docs_dir):
            doc_dir =  os.path.join(docs_dir, name)
            if name.startswith('.'):
                continue
            elif os.path.isfile(doc_dir):
                if name.endswith(".json"):
                    doc = self.ui.read_json(doc_dir)
                    docid, ext = os.path.splitext(name)
                    
                    doc.setdefault('_id', docid)
                    doc.setdefault('couchapp', {})
                    docs.append(doc)
            else:
                doc = { '_id': name }
                manifest = []
                attach_dir = self.ui.rjoin(doc_dir, '_attachments')
                doc.update(self.dir_to_fields(doc_dir, manifest=manifest))
                doc.setdefault('couchapp', {})
                if not 'couchapp' in doc:
                    doc['couchapp'] = {}
                doc['couchapp'].update({ 'manifest': manifest })
                self.attachments(doc, attach_dir, name)
                
                docs.append(doc)
        return docs

            
    def fs_to_designdoc(self, app_name, pre_callback=None, post_callback=None):            
        """
        function used to get design_doc from app_dir. It return a dict with all
        properties. attachements are file handles and shoul be processed before 
        saving design_doc to couchdb.
        
        
        :attr app_name: string, name of applicaton. used to create design doc id.
        :attr pre_callback: callable. Used to proccess aapp_dir and add default value 
        to design_doc before retrieving properties from app_dir.
        
        ex.
            .. code-block:: python
                def mycallbacl(app_dir, app_name, design_doc, verbose=verbose):
                    pass
                    
        
        :attr post_callback: callable, like pre_callback but called after the app_dir
        was processed.
        :attr verbose: boolean, default False
        
        :return: dict, design_doc or error.
        
        """
        # init variables
        manifest = []
        design_doc = {}
        objects = {}
        docid = design_doc['_id'] = '_design/%s' % app_name
        attach_dir = self.ui.rjoin(self.app_dir, '_attachments')
        
        # what we do before retrieving design_doc from app_dir        
        if pre_callback and callable(pre_callback):
            pre_callback(self.app_dir, app_name, design_doc,
                    verbose=verbose)
        
        # get fields
        design_doc.update(self.dir_to_fields(self.app_dir, manifest=manifest))
        
        if not 'couchapp' in design_doc:
            design_doc['couchapp'] = {}
            
        for funs in ['shows', 'lists', 'updates', 'filters']:
            if funs in design_doc:
                package_shows(design_doc, design_doc[funs], self.app_dir, objects, self.ui)
            
        if 'validate_doc_update' in design_doc:
            tmp_dict = dict(validate_doc_update=design_doc["validate_doc_update"])
            package_shows(design_doc, tmp_dict, self.app_dir, objects, self.ui)
            design_doc.update(tmp_dict)

        if 'views' in design_doc:
            # clean views
            # we remove empty views and malformed from the list
            # of pushed views. We also clean manifest
            views = {}
            dmanifest = {}
            for i, fname in enumerate(manifest):
                if fname.startswith("views/") and fname != "views/":
                    name, ext = os.path.splitext(fname)
                    if name.endswith('/'):
                        name = name[:-1]
                    dmanifest[name] = i
            
            for vname, value in design_doc['views'].iteritems():
                if value and isinstance(value, dict):
                    views[vname] = value
                else:
                    del manifest[dmanifest["views/%s" % vname]]
            design_doc['views'] = views
            package_views(design_doc, design_doc["views"], self.app_dir, objects, self.ui)
            
        couchapp = design_doc.get('couchapp', {})
        couchapp.update({
            'manifest': manifest,
            'objects': objects
        })
        design_doc['couchapp'] = couchapp
        self.attachments(design_doc, attach_dir, docid)
        
        self.vendor_attachments(design_doc, docid)
        
        # what we do after retrieving design_doc from app_dir 
        if pre_callback and callable(pre_callback):
            pre_callback(self.app_dir, app_name, design_doc,
                    verbose=self.ui.verbose)
            
        return design_doc
        
    def dir_to_fields(self, current_dir='', depth=0,
            manifest=[]):
        fields={}
        if not current_dir:
            current_dir = self.app_dir
        for name in self.ui.listdir(current_dir):
            current_path = self.ui.rjoin(current_dir, name)
            rel_path = self.ui.relpath(current_path, self.app_dir)
            if name.startswith("."):
                continue
            elif depth == 0 and name.startswith('_'):
                # files starting with "_" are always "special"
                continue
            elif name == '_attachments':
                continue
            elif depth == 0 and (name == 'couchapp' or name == 'couchapp.json'):
                # we are in app_meta
                if name == "couchapp":
                    manifest.append('%s/' % rel_path)
                    content = self.dir_to_fields(current_path,
                        depth=depth+1, manifest=manifest)
                else:
                    manifest.append(rel_path)
                    content = self.ui.read_json(current_path)
                    if not isinstance(content, dict):
                        content = { "meta": content }
                if 'signatures' in content:
                    del content['signatures']

                if 'manifest' in content:
                    del content['manifest']

                if 'objects' in content:
                    del content['objects']
                
                if 'length' in content:
                    del content['length']

                if 'couchapp' in fields:
                    fields['couchapp'].update(content)
                else:
                    fields['couchapp'] = content
            elif self.ui.isdir(current_path):
                manifest.append('%s/' % rel_path)
                fields[name] = self.dir_to_fields(current_path,
                        depth=depth+1, manifest=manifest)
            else:
                if self.ui.verbose >= 2:
                    self.ui.logger.info("push %s" % rel_path)               
                content = ''
                try:
                    content = self.ui.read(current_path)
                except UnicodeDecodeError, e:
                    self.ui.logger.error("%s isn't encoded in utf8" % current_path)
                    content = self.ui.read(current_path, utf8=False)
                    try:
                        content.encode('utf-8')
                    except UnicodeError, e:
                        self.ui.logger.error("plan B didn't work, %s is a binary" % current_path)
                        self.ui.logger.error("use plan C: encode to base64")   
                        content = "base64-encoded;%s" % base64.b64encode(content)
                        
                if name.endswith('.json'):
                    try:
                        content = json.loads(content)
                    except ValueError:
                        if self.ui.verbose >= 2:
                            self.ui.logger.error("Json invalid in %s" % current_path)
                
                # remove extension
                name, ext = os.path.splitext(name)
                if name in fields and ext in ('.txt'):
                    if self.ui.verbose >= 2:
                        self.ui.logger.error("%(name)s is already in properties. Can't add (%(name)s%(ext)s)" % {
                            "name": name, "ext": ext })
                else:
                    manifest.append(rel_path)
                    fields[name] = content
        return fields
        
    def vendor_attachments(self, design_doc,  docid):
        vendor_dir = self.ui.rjoin(self.app_dir, 'vendor')
        if not self.ui.isdir(vendor_dir):
            if self.ui.verbose >=2:
                self.ui.logger.info("%s don't exist" % vendor_dir)
            return
            
        for name in self.ui.listdir(vendor_dir):
            current_path = self.ui.rjoin(vendor_dir, name)
            if self.ui.isdir(current_path):
                attach_dir = self.ui.rjoin(current_path, '_attachments')
                if self.ui.isdir(attach_dir):
                    self.attachments(design_doc, attach_dir, docid, vendor=name)
                    
    def attachments(self, doc, attach_dir, docid, vendor=None):
        # get attachments
        _signatures = {}
        _attachments = {}
        _length = {}
        all_signatures = {}
        for root, dirs, files in self.ui.walk(attach_dir):
            for dirname in dirs:
                if dirname.startswith('.'):
                    dirs.remove(dirname)
            if files:
                for filename in files:
                    if filename.startswith('.'):
                        continue
                    else:
                        file_path = self.ui.rjoin(root, filename) 
                        name = self.ui.relpath(file_path, attach_dir)
                        if vendor is not None:
                            name = self.ui.rjoin('vendor', vendor, name)
                        _signatures[name] = self.ui.sign(file_path)
                        _attachments[name] = file_path
                        _length[name] = int(os.path.getsize(file_path))
        
        for prop in ('couchapp', '_attachments'):
            if not prop in doc:
                doc[prop] = {}
            
        if not 'signatures' in doc['couchapp']:
            doc['couchapp']['signatures'] = {}
            
        if not 'length' in doc['couchapp']:
            doc['couchapp']['length'] = {}
            
        doc['_attachments'].update(_attachments)
        doc['couchapp']['signatures'].update(_signatures)
        doc['couchapp']['length'].update(_length)
        
    def designdoc_to_fs(self, db, design_doc):
        """
        Clone an application from a design_doc given.
        
        :attr design_doc: dict, the design doc retrieved from couchdb
        if something was wrong.
        """
        
        app_name = get_appname(design_doc['_id'])
        docid = design_doc['_id']
            
        metadata = design_doc.get('couchapp', {})
        
        # get manifest
        manifest = metadata.get('manifest', {})

        # get signatures
        signatures = metadata.get('signatures', {})

        # get objects refs
        objects = metadata.get('objects', {})

        # create files from manifest
        if manifest:
            for filename in manifest:
                if self.ui.verbose >=2:
                    self.ui.logger.info("clone property: %s" % filename)
                file_path = self.ui.rjoin(self.app_dir, filename)
                if filename.endswith('/'): 
                    if not self.ui.isdir(file_path):
                        self.ui.makedirs(file_path)
                elif filename == "couchapp.json":
                    continue
                else:
                    parts = self.ui.split_path(filename)
                    fname = parts.pop()
                    v = design_doc
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
                            _ref = md5(to_bytestring(content)).hexdigest()
                            if objects and _ref in objects:
                                content = objects[_ref]
                                
                            if content.startswith('base64-encoded;'):
                                content = base64.b64decode(content[15:])

                        if fname.endswith('.json'):
                            content = json.dumps(content)

                        del v[last_key]

                        # make sure file dir have been created
                        file_dir = self.ui.dirname(file_path)
                        if not self.ui.isdir(file_dir):
                            self.ui.makedirs(file_dir)
                        
                        self.ui.write(file_path, content)

                        # remove the key from design doc
                        temp = design_doc
                        for key2 in parts:
                            if key2 == key:
                                if not temp[key2]:
                                    del temp[key2]
                                break
                            temp = temp[key2]
                            
        
        # second pass for missing key or in case
        # manifest isn't in app
        for key in design_doc.iterkeys():
            if key.startswith('_'): 
                continue
            elif key in ('couchapp'):
                app_meta = copy.deepcopy(design_doc['couchapp'])
                if 'signatures' in app_meta:
                    del app_meta['signatures']
                if 'manifest' in app_meta:
                    del app_meta['manifest']
                if 'objects' in app_meta:
                    del app_meta['objects']
                if 'lenght' in app_meta:
                    del app_meta['lenght']
                if app_meta:
                    couchapp_file = self.ui.rjoin(self.app_dir, 'couchapp.json')
                    self.ui.write_json(couchapp_file, app_meta)
            elif key in ('views'):
                vs_dir = self.ui.rjoin(self.app_dir, key)
                if not self.ui.isdir(vs_dir):
                    self.ui.makedirs(vs_dir)
                for vsname, vs_item in design_doc[key].iteritems():
                    vs_item_dir = self.ui.rjoin(vs_dir, vsname)
                    if not self.ui.isdir(vs_item_dir):
                        self.ui.makedirs(vs_item_dir)
                    for func_name, func in vs_item.iteritems():
                        filename = self.ui.rjoin(vs_item_dir, '%s.js' % 
                                func_name)
                        self.ui.write(filename, func)
                        if self.ui.verbose >=2:
                            self.ui.logger.info("clone view not in manifest: %s" % filename)
            elif key in ('shows', 'lists'):
                show_path = self.ui.rjoin(self.app_dir, key)
                if not self.ui.isdir(show_path):
                    self.ui.makedirs(show_path)
                for func_name, func in design_doc[key].iteritems():
                    filename = self.ui.rjoin(show_path, '%s.js' % 
                            func_name)
                    self.ui.write(filename, func)
                    if self.ui.verbose >=2:
                        self.ui.logger.info("clone show or list not in manifest: %s" % filename)
            else:
                file_dir = self.ui.rjoin(self.app_dir, key)
                if self.ui.exists(file_dir):
                    continue
                else:
                    if self.ui.verbose >=2:
                        self.ui.logger.info("clone property not in manifest: %s" % key)
                    if isinstance(design_doc[key], (list, tuple,)):
                        self.ui.write_json(file_dir + ".json", design[key])
                    elif isinstance(design_doc[key], dict):
                        if not self.ui.isdir(file_dir):
                            self.ui.makedirs(file_dir)
                        for field, value in design_doc[key].iteritems():
                            field_path = self.ui.rjoin(file_dir, field)
                            if isinstance(value, basestring):
                                if value.startswith('base64-encoded;'):
                                    value = base64.b64decode(content[15:])
                                self.ui.write(field_path, value)
                            else:
                                self.ui.write_json(field_path + '.json', value)        
                    else:
                        value = design_doc[key]
                        if not isinstance(value, basestring):
                            value = str(value)
                        self.ui.write(file_dir, value)
   

        # get attachments
        if '_attachments' in design_doc:
            attach_dir = self.ui.rjoin(self.app_dir, '_attachments')
            if not self.ui.isdir(attach_dir):
                self.ui.makedirs(attach_dir)
                
            for filename in design_doc['_attachments'].iterkeys():
                if filename.startswith('vendor'):
                    attach_parts = self.ui.split_path(filename)
                    vendor_attach_dir = self.ui.rjoin(self.app_dir, attach_parts.pop(0),
                            attach_parts.pop(0), '_attachments')
                    file_path = self.ui.rjoin(vendor_attach_dir, *attach_parts)
                else:
                    file_path = self.ui.rjoin(attach_dir, filename)
                current_dir = self.ui.dirname(file_path)
                if not self.ui.isdir(current_dir):
                    self.ui.makedirs(current_dir)
        
                if signatures.get(filename) != self.ui.sign(file_path):
                    self.ui.write(file_path, db.get_attachment(docid, filename))
                    if self.ui.verbose>=2:
                        self.ui.logger.info("clone attachment: %s" % filename)
                        
    def index_url(self, uri, app_name, attach_path, index):
        if index:
            return "%s/%s/%s/%s" % (uri, '_design', app_name, index)
        else:
            index_fpath = self.ui.rjoin(attach_path, 'index.html')
            if self.ui.isfile(index_fpath):
                return "%s/%s/%s/%s" % (uri, '_design', app_name, 'index.html')
            else:
                return False
