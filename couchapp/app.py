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
import copy
from hashlib import md5
import os
import os.path
try:
    import json
except ImportError:
    import couchapp.simplejson as json
    
from couchapp.errors import AppError
import couchapp.couchdbclient as client
from couchapp.utils import to_bytestring
import couchapp.generator as generator
from couchapp.vendor import Vendor
import couchapp.localdoc as localdoc


def document(ui, path='', create=False, docid=None):
    doc = localdoc.instance(ui, path, create=create, docid=docid)
    return doc

if os.name == 'nt':
    def _replace_slash(name):
        return name.replace("/", "\\")
else:
    def _replace_slash(name):
        return name

def clone(ui, source, dest=None, rev=None):
    """
    Clone an application from a design_doc given.
    
    :attr design_doc: dict, the design doc retrieved from couchdb
    if something was wrong.
    """
    try:
        dburl, docid = source.split('_design/')
    except ValueError:
        raise AppError("%s isn't a valid source" % source)

    if not dest:
        dest = docid
   
    path = os.path.normpath(os.path.join(os.getcwd(), dest))
    if not os.path.exists(path):
        os.makedirs(path)

    db = client.Database(ui, dburl)    
    if not rev:
        doc = db.get_doc("_design/%s" % docid)
    else:
        doc = db.get_doc("_design/%s" % docid, rev=rev)
    docid = doc['_id']
        
    
    metadata = doc.get('couchapp', {})
    
    # get manifest
    manifest = metadata.get('manifest', {})

    # get signatures
    signatures = metadata.get('signatures', {})

    # get objects refs
    objects = metadata.get('objects', {})

    # create files from manifest
    if manifest:
        for filename in manifest:
            if ui.verbose >=2:
                ui.logger.info("clone property: %s" % filename)
            filepath = os.path.join(path, filename)
            if filename.endswith('/'): 
                if not os.path.isdir(filepath):
                    os.makedirs(filepath)
            elif filename == "couchapp.json":
                continue
            else:
                parts = ui.split_path(filename)
                fname = parts.pop()
                v = doc
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
                        content = json.dumps(content).encode('utf-8')

                    del v[last_key]

                    # make sure file dir have been created
                    filedir = os.path.dirname(filepath)
                    if not os.path.isdir(filedir):
                        os.makedirs(filedir)
                    
                    ui.write(filepath, content)

                    # remove the key from design doc
                    temp = doc
                    for key2 in parts:
                        if key2 == key:
                            if not temp[key2]:
                                del temp[key2]
                            break
                        temp = temp[key2]
                        
    
    # second pass for missing key or in case
    # manifest isn't in app
    for key in doc.iterkeys():
        if key.startswith('_'): 
            continue
        elif key in ('couchapp'):
            app_meta = copy.deepcopy(doc['couchapp'])
            if 'signatures' in app_meta:
                del app_meta['signatures']
            if 'manifest' in app_meta:
                del app_meta['manifest']
            if 'objects' in app_meta:
                del app_meta['objects']
            if 'length' in app_meta:
                del app_meta['length']
            if app_meta:
                couchapp_file = os.path.join(path, 'couchapp.json')
                ui.write_json(couchapp_file, app_meta)
        elif key in ('views'):
            vs_dir = os.path.join(path, key)
            if not os.path.isdir(vs_dir):
                os.makedirs(vs_dir)
            for vsname, vs_item in doc[key].iteritems():
                vs_item_dir = os.path.join(vs_dir, vsname)
                if not os.path.isdir(vs_item_dir):
                    os.makedirs(vs_item_dir)
                for func_name, func in vs_item.iteritems():
                    filename = os.path.join(vs_item_dir, '%s.js' % 
                            func_name)
                    ui.write(filename, func)
                    if ui.verbose >=2:
                        ui.logger.info("clone view not in manifest: %s" % filename)
        elif key in ('shows', 'lists', 'filter', 'update'):
            showpath = os.path.join(path, key)
            if not os.path.isdir(showpath):
                os.makedirs(showpath)
            for func_name, func in doc[key].iteritems():
                filename = os.path.join(showpath, '%s.js' % 
                        func_name)
                ui.write(filename, func)
                if ui.verbose >=2:
                    ui.logger.info("clone show or list not in manifest: %s" % filename)
        else:
            filedir = os.path.join(path, key)
            if os.path.exists(filedir):
                continue
            else:
                if ui.verbose >=2:
                    ui.logger.info("clone property not in manifest: %s" % key)
                if isinstance(doc[key], (list, tuple,)):
                    ui.write_json(filedir + ".json", doc[key])
                elif isinstance(doc[key], dict):
                    if not os.path.isdir(filedir):
                        os.makedirs(filedir)
                    for field, value in doc[key].iteritems():
                        fieldpath = os.path.join(filedir, field)
                        if isinstance(value, basestring):
                            if value.startswith('base64-encoded;'):
                                value = base64.b64decode(content[15:])
                            ui.write(fieldpath, value)
                        else:
                            ui.write_json(fieldpath + '.json', value)        
                else:
                    value = doc[key]
                    if not isinstance(value, basestring):
                        value = str(value)
                    ui.write(filedir, value)

    # save id
    idfile = os.path.join(path, '_id')
    ui.write(idfile, doc['_id'])
  
    ui.write_json(os.path.join(path, '.couchapprc'), {})

    if '_attachments' in doc:  # process attachments
        attachdir = os.path.join(path, '_attachments')
        if not os.path.isdir(attachdir):
            os.makedirs(attachdir)
            
        for filename in doc['_attachments'].iterkeys():
            if filename.startswith('vendor'):
                attach_parts = ui.split_path(filename)
                vendor_attachdir = os.path.join(path, attach_parts.pop(0),
                        attach_parts.pop(0), '_attachments')
                filepath = os.path.join(vendor_attachdir, *attach_parts)
            else:
                filepath = os.path.join(attachdir, filename)
            filepath = _replace_slash(filepath)
            currentdir = os.path.dirname(filepath)
            if not os.path.isdir(currentdir):
                os.makedirs(currentdir)
    
            if signatures.get(filename) != ui.sign(filepath):
                resp = db.fetch_attachment(docid, filename, stream=True)
                f = open(filepath, 'wb')
                while 1:
                    data = resp.read(16384)
                    if not data: break
                    f.write(data)
                f.close()
                if ui.verbose>=2:
                    ui.logger.info("clone attachment: %s" % filename)
                    
def generate(ui, path, kind, name, **opts):
    if kind not in ["app", "view", "list", "show", 'filter', 'function', 'vendor', 'update']:
        raise AppError("Can't generate %s in your couchapp. generator is unknown" % kind)

    if kind == "app":
        generator.generate_app(ui, path, template=opts.get("template"), 
                        create=opts.get('create', False))
    else:
        if name is None:
            raise AppError("Can't generate %s function, name is missing" % kind)
        generator.generate_function(ui, path, kind, name, opts.get("template"))
        
        
def vendor_install(ui, dest, source, *args, **opts):
    vendor = Vendor(ui)
    vendor.install(dest, source, *args, **opts)
    
def vendor_update(ui, dest, name=None, *args, **opts):
    vendor = Vendor(ui)
    vendor.update(dest, name, *args, **opts)
    
