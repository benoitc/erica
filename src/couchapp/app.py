#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# Copyright 2009 Benoit Chesneau <benoitc@e-engura.org>
#
# This software is licensed as described in the file LICENSE, which
# you should have received as part of this distribution.
#

import copy
from hashlib import md5
import os
import re
import shutil
import sys

from couchapp.contrib import simplejson as json
from couchapp.errors import *
from couchapp.macros import package_views, package_shows
from couchapp.utils import *

class CouchApp(object):
    """ app object. used to push/clone/init/create a couchapp """
    
    default_locations = (
        ("template", ['app-template', '../../app-template']),
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

        rc_file = '%s/.couchapprc' % self.app_dir
        if not self.ui.isfile(rc_file):
            self.ui.write_json(rc_file, default_conf)
        elif self.ui.verbose:
            raise AppError("CouchApp already initialized in %s." % self.app_dir)

    def generate(self):
        """ Generates a CouchApp in app_dir 
        
        :attr verbose: boolean, default False
        :return: boolean, dict. { 'ok': True } if ok, { 'ok': False, 'error': message } 
        if something was wrong.
        """
        
        locations = {}
        for location, paths in self.default_locations:
            found = False
            location_dir = ""
            for path in paths:
                location_dir = os.path.normpath(self.ui.rjoin(
                    self.ui.dirname(__file__), path))
                if self.ui.isdir(location_dir):
                    found = True
                    break
            if found:
                if location == "template":
                    dest_dir = self.app_dir
                else:
                    dest_dir = self.ui.rjoin(self.app_dir, 'vendor')
                try:
                    shutil.copytree(location_dir, dest_dir)
                except OSError, e:
                    errno, message = e
                    raise AppError("Can't create a CouchApp in %s: %s" % (
                            self.app_dir, message))
            else:
                raise AppError("Can't create a CouchApp in %s: default template not found." % (
                        self.app_dir))
        self.initialize()
        
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
        
    def push(self, dbstring, app_name, **kwargs):
        """Pushes the app specified to the CouchDB instance
        
        :attr dbstring: string, db url or db environment name.
        :attr app_name: name of app to push. 
        :attr verbose: boolean, default is False
        """
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
        
        # we process attachments later
        del new_doc['_attachments']
        if 'signatures' in new_doc['couchapp']:
            del new_doc['couchapp']['signatures']

        for db in self.ui.get_db(dbstring):
            if self.ui.verbose >= 1:
                self.ui.logger.info("Pushing CouchApp in %s to design doc:\n%s/%s" % (self.app_dir,
                    db.resource.uri, docid))
            
            index_url = self.index_url(db.resource.uri, app_name, attach_dir, index)
            if index_url:
                self.ui.logger.info("Visit your CouchApp here:\n%s" % index_url)

            if docid in db:
                design = db[docid]
                _app_meta = design.get('couchapp', {})

                new_doc['couchapp'] ['signatures'] = _app_meta.get('signatures', {})

                new_doc.update({
                    '_rev': design['_rev'],
                    '_attachments': design.get('_attachments', {})
                })

            db[docid] = new_doc
            print "là"
            self.send_attachments(db, design_doc)

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
                elif _signatures[filename] == metadata['signatures'][filename]:
                    del attachments[filename]
        for filename, value in attachments.iteritems():
            content_length = _length.get(filename, None)
            if self.ui.verbose >= 2:
                self.ui.logger.info("Attaching %s (%s)" % (filename, content_length))
            
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
        design['couchapp'].update({'signatures': all_signatures})
        db[docid] = design
            
    def fs_to_designdoc(self, app_name, pre_callback=None, post_callback=None):            
        """
        function used to get design_doc from app_dir. It return a dict with all
        properties. attachements are file handles and shoul be processed before saving
        design_doc to couchdb.
        
        
        :attr app_name: string, name of applicaton. used to create design doc id.
        :attr pre_callback: callable. Used to proccess aapp_dir and add default value to design_doc
        before retrieving properties from app_dir.
        
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
            
        if 'shows' in design_doc:
            package_shows(design_doc, design_doc['shows'], self.app_dir, objects, self.ui)

        if 'lists' in design_doc:
            package_shows(design_doc, design_doc['lists'], self.app_dir, objects, self.ui)

        if 'validate_doc_update' in design_doc:
            tmp_dict = dict(validate_doc_update=design_doc["validate_doc_update"])
            package_shows(design_doc, tmp_dict, self.app_dir, objects, self.ui)
            design_doc.update(tmp_dict)

        if 'views' in design_doc:
            package_views(design_doc, design_doc["views"], self.app_dir, objects, self.ui)
            
        couchapp = design_doc.get('couchapp', False)
        design_doc.update({
            'couchapp': {
                'manifest': manifest,
                'objects': objects
            }
        })
                    
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
            rel_path = current_path.split("%s/" % self.app_dir)[1]
            if name.startswith('.'):
                continue
            elif name.startswith('_'):
                # files starting with "_" are always "special"
                continue
            elif depth == 0 and name in ('couchapp', 'couchapp.json'):
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
                    self.ui.logger.error(str(e))
                if name.endswith('.json'):
                    try:
                        content = json.loads(content)
                    except ValueError:
                        if self.ui.verbose >= 2:
                            self.ui.logger.error("Json invalid in %s" % current_path)
                
                # remove extension
                name, ext = os.path.splitext(name)
                if name in fields:
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
            if files:
                for filename in files:
                    if filename.startswith('.'):
                        continue
                    else:
                        file_path = self.ui.rjoin(root, filename) 
                        name = file_path.split('%s/' % attach_dir)[1]
                        if vendor is not None:
                            name = self.ui.rjoin('vendor/%s' % vendor, name)
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
                    print "clone property: %s" % filename
                file_path = self.ui.rjoin(self.app_dir, filename)
                if filename.endswith('/'): 
                    if not self.ui.isdir(file_path):
                        self.ui.makedirs(file_path)
                elif filename == "couchapp.json":
                    continue
                else:
                    parts = filename.split('/')
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
                dir = self.ui.rjoin(self.app_dir, key)
                if not self.ui.isdir(dir):
                    self.ui.makedirs(dir)
                for func_name, func in design_doc[key].iteritems():
                    filename = self.ui.rjoin(dir, '%s.js' % 
                            func_name)
                    self.ui.write(filename, func)
                    if self.ui.verbose >=2:
                        self.ui.logger.info("clone show or list not in manifest: %s" % filename)
            else:
                file_dir = self.ui.rjoin(self.app_dir, key)
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
                    attach_parts = filename.split('/')
                    vendor_attach_dir = self.ui.rjoin(self.app_dir, attach_parts.pop(0),
                            attach_parts.pop(0), '_attachments')
                    file_path = self.ui.rjoin(vendor_attach_dir, '/'.join(attach_parts))
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
