#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# Copyright 2009 Benoit Chesneau <benoitc@e-engura.org>
#
# This software is licensed as described in the file LICENSE, which
# you should have received as part of this distribution.
#

import copy
import os
import re
import shutil
import sys

# python 2.6
try:
    import json 
except ImportError:
    import simplejson as json 


from couchapp.macros import package_views, package_shows
from couchapp.utils import _md5, to_bytestring
from couchapp.utils import *

__all__ = ['Couchapp']


DEFAULT_LOCATIONS = (
    ("template", ['app-template', '../../app-template']),
    ("vendor", ['vendor', '../../vendor'])
)


class Couchapp(object):
    """ Couchapp object. used to clone/init/create a couchapp """
        
    def __init__(self, app_dir, generate=False, verbose=False):
        """
        Constructor for Couchapp object.
        
        :attr app_dir: string,  path to app dir
        :attr generate: boolean, do we generate application in app_dir ?
        :attr verbose: boolean, default False
        
        """
        self.app_dir = app_dir
        if generate:
            self.generate(verbose=verbose)
            
    def initialize(self, default_conf=None, verbose=False):
        """
        Initialize an app. It basically create the .couchapprc file
        and add default conf.
        
        :attr default_conf: dict, default configuration
        :attr verbose: boolean, default False
        :return: boolean, dict. { 'ok': True } if ok, { 'ok': False, 'error': message } 
        if something was wrong?
        """
        
        if not os.path.isdir(self.app_dir):
            message = "%s directory doesn't exist." % self.app_dir
            return error(message, verbose)
        
        default_conf = default_conf or {}

        rc_file = '%s/.couchapprc' % self.app_dir
        if not os.path.isfile(rc_file):
            write_json(rc_file, default_conf)
        else:
            message = "CouchApp already initialized in %s." % app_dir
            return error(message, verbose)
        return is_ok()
            
    def generate(self, verbose=False):
        """ Generates a CouchApp in app_dir 
        
        :attr verbose: boolean, default False
        :return: boolean, dict. { 'ok': True } if ok, { 'ok': False, 'error': message } 
        if something was wrong.
        """
        
        locations = {}
        for location, paths in DEFAULT_LOCATIONS:
            found = False
            location_dir = ""
            for path in paths:
                location_dir = os.path.normpath(os.path.join(
                    os.path.dirname(__file__), path))
                if os.path.isdir(location_dir):
                    found = True
                    break
            if found:
                if location == "template":
                    dest_dir = self.app_dir
                else:
                    dest_dir = os.path.join(self.app_dir, 'vendor')
                try:
                    shutil.copytree(location_dir, dest_dir)
                except OSError, e:
                    errno, message = e
                    return error("Can't create a CouchApp in %s: %s" % (
                                self.app_dir, message), verbose)
            else:
                return error("Can't create a CouchApp in %s: default template not found." % (
                            self.app_dir), verbose)
                            
        return self.initialize(verbose=verbose)
        
    def clone(self, design_doc, verbose=None):
        """
        Clone an application from a design_doc given.
        
        :attr design_doc: dict, the design doc retrieved from couchdb
        :attr verbose: boolean, default False
        
        :return: boolean, dict. { 'ok': True } if ok, { 'ok': False, 'error': message } 
        if something was wrong.
        """
        
        app_name = get_appname(design_doc['_id'])

        self.app_dir = os.path.join(self.app_dir, app_name)

        if not os.path.isdir(self.app_dir):
            os.makedirs(self.app_dir)
            
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
                if verbose >=2:
                    print "clone property: %s" % filename
                file_path = os.path.join(self.app_dir, filename)
                if filename.endswith('/'): 
                    if not os.path.isdir(file_path):
                        os.makedirs(file_path)
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
                    couchapp_file = os.path.join(self.app_dir, 'couchapp.json')
                    write_json(couchapp_file, app_meta)
            elif key in ('views'):
                vs_dir = os.path.join(self.app_dir, key)
                if not os.path.isdir(vs_dir):
                    os.makedirs(vs_dir)
                for vsname, vs_item in design_doc[key].iteritems():
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
                dir = os.path.join(self.app_dir, key)
                if not os.path.isdir(dir):
                    os.makedirs(dir)
                for func_name, func in design_doc[key].iteritems():
                    filename = os.path.join(dir, '%s.js' % 
                            func_name)
                    open(filename, 'w').write(func)
                    if verbose >=2:
                        print "clone show or list not in manifest: %s" % filename
            else:
                file_dir = os.path.join(self.app_dir, key)
                if verbose >=2:
                    print "clone property not in manifest: %s" % key
                if isinstance(design_doc[key], (list, tuple,)):
                    write_json(file_dir + ".json", design[key])
                elif isinstance(design_doc[key], dict):
                    if not os.path.isdir(file_dir):
                        os.makedirs(file_dir)
                    for field, value in design_doc[key].iteritems():
                        field_path = os.path.join(file_dir, field)
                        if isinstance(value, basestring):
                            write_content(field_path, value)
                        else:
                            write_json(field_path + '.json', value)        
                else:
                    value = design_doc[key]
                    if not isinstance(value, basestring):
                        value = str(value)
                    write_content(file_dir, value)
   

        # get attachments
        if '_attachments' in design_doc:
            attach_dir = os.path.join(self.app_dir, '_attachments')
            if not os.path.isdir(attach_dir):
                os.makedirs(attach_dir)
                
            for filename in design_doc['_attachments'].iterkeys():
                if filename.startswith('vendor'):
                    attach_parts = filename.split('/')
                    vendor_attach_dir = os.path.join(self.app_dir, attach_parts.pop(0),
                            attach_parts.pop(0), '_attachments')
                    file_path = os.path.join(vendor_attach_dir, '/'.join(attach_parts))
                else:
                    file_path = os.path.join(attach_dir, filename)
                current_dir = os.path.dirname(file_path)
                if not os.path.isdir(current_dir):
                    os.makedirs(current_dir)
        
                if signatures.get(filename) != sign_file(file_path):
                    content = design_doc['_attachments'][filename].read()
                    write_content(file_path, content)
                    if verbose>=2:
                        print "clone attachment: %s" % filename
                        
        return { 'ok': True }
            
    def to_designdoc(self, app_name, pre_callback=None, post_callback=None,
                    verbose=False):            
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
        attach_dir = os.path.join(self.app_dir, '_attachments')
        
        # what we do before retrieving design_doc from app_dir        
        if pre_callback and callable(pre_callback):
            pre_callback(self.app_dir, app_name, design_doc,
                    verbose=verbose)
        
        # get fields
        design_doc.update(self.dir_to_fields(self.app_dir, manifest=manifest,
                verbose=verbose))
        
        if not 'couuchapp' in design_doc:
            design_doc['couchapp'] = {}
            
        if 'shows' in design_doc:
            package_shows(design_doc, design_doc['shows'], self.app_dir, objects, verbose=verbose)

        if 'lists' in design_doc:
            package_shows(design_doc, design_doc['lists'], self.app_dir, objects, verbose=verbose)

        if 'views' in design_doc:
            package_views(design_doc, design_doc["views"], self.app_dir, objects, verbose=verbose)
            
        couchapp = design_doc.get('couchapp', False)
        design_doc.update({
            'couchapp': {
                'manifest': manifest,
                'objects': objects
            }
        })
                    
        self.attachments(design_doc, attach_dir, docid, verbose=verbose)
        self.vendor_attachments(design_doc, docid, verbose=verbose)
        
        # what we do after retrieving design_doc from app_dir 
        if pre_callback and callable(pre_callback):
            pre_callback(self.app_dir, app_name, design_doc,
                    verbose=verbose)
            
        return design_doc
        
    def dir_to_fields(self, current_dir='', depth=0,
            manifest=[], verbose=False):
        fields={}
        if not current_dir:
            current_dir = self.app_dir
        for name in os.listdir(current_dir):
            current_path = os.path.join(current_dir, name)
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
                fields[name] = self.dir_to_fields(current_path,
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
        
    def vendor_attachments(self, design_doc,  docid, verbose):
        vendor_dir = os.path.join(self.app_dir, 'vendor')
        if not os.path.isdir(vendor_dir):
            return
            
        for name in os.listdir(vendor_dir):
            current_path = os.path.join(vendor_dir, name)
            if os.path.isdir(current_path):
                attach_dir = os.path.join(current_path, '_attachments')
                if os.path.isdir(attach_dir):
                    self.push_directory(design_doc, attach_dir, docid, verbose, 
                                    vendor=name)
                    
    def attachments(self, doc, attach_dir, docid, verbose=False, vendor=None):
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
                        _signatures[name] = sign_file(file_path)
                        _attachments[name] = open(file_path, 'rb')
        
        for prop in ('couchapp', '_attachments'):
            if not prop in doc:
                doc[prop] = {}
            
        if not 'signatures' in doc['couchapp']:
            doc['couchapp']['signatures'] = {}
            
        doc['_attachments'].update(_attachments)
        doc['couchapp']['signatures'].update(_signatures)
