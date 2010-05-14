# -*- coding: utf-8 -*-
#
# This file is part of couchapp released under the Apache 2 license. 
# See the NOTICE for more information.

from __future__ import with_statement

import base64
import copy
from hashlib import md5
import logging
import os
import os.path
try:
    import json
except ImportError:
    import couchapp.simplejson as json
    
from couchapp.errors import AppError
from couchapp import client
from couchapp import util

logger = logging.getLogger(__name__)


if os.name == 'nt':
    def _replace_slash(name):
        return name.replace("/", "\\")
else:
    def _replace_slash(name):
        return name

def clone(source, dest=None, rev=None):
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

    db = client.Database(dburl[:-1])    
    if not rev:
        doc = db.open_doc("_design/%s" % docid)
    else:
        doc = db.open_doc("_design/%s" % docid, rev=rev)
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
            logger.debug("clone property: %s" % filename)
            filepath = os.path.join(path, filename)
            if filename.endswith('/'): 
                if not os.path.isdir(filepath):
                    os.makedirs(filepath)
            elif filename == "couchapp.json":
                continue
            else:
                parts = util.split_path(filename)
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
                        _ref = md5(util.to_bytestring(content)).hexdigest()
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
                    
                    util.write(filepath, content)

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
                util.write_json(couchapp_file, app_meta)
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
                    util.write(filename, func)
                    logger.warning("clone view not in manifest: %s" % filename)
        elif key in ('shows', 'lists', 'filter', 'update'):
            showpath = os.path.join(path, key)
            if not os.path.isdir(showpath):
                os.makedirs(showpath)
            for func_name, func in doc[key].iteritems():
                filename = os.path.join(showpath, '%s.js' % 
                        func_name)
                util.write(filename, func)
                logger.warning(
                    "clone show or list not in manifest: %s" % filename)
        else:
            filedir = os.path.join(path, key)
            if os.path.exists(filedir):
                continue
            else:
                logger.warning("clone property not in manifest: %s" % key)
                if isinstance(doc[key], (list, tuple,)):
                    util.write_json(filedir + ".json", doc[key])
                elif isinstance(doc[key], dict):
                    if not os.path.isdir(filedir):
                        os.makedirs(filedir)
                    for field, value in doc[key].iteritems():
                        fieldpath = os.path.join(filedir, field)
                        if isinstance(value, basestring):
                            if value.startswith('base64-encoded;'):
                                value = base64.b64decode(content[15:])
                            util.write(fieldpath, value)
                        else:
                            util.write_json(fieldpath + '.json', value)        
                else:
                    value = doc[key]
                    if not isinstance(value, basestring):
                        value = str(value)
                    util.write(filedir, value)

    # save id
    idfile = os.path.join(path, '_id')
    util.write(idfile, doc['_id'])
  
    util.write_json(os.path.join(path, '.couchapprc'), {})

    if '_attachments' in doc:  # process attachments
        attachdir = os.path.join(path, '_attachments')
        if not os.path.isdir(attachdir):
            os.makedirs(attachdir)
            
        for filename in doc['_attachments'].iterkeys():
            if filename.startswith('vendor'):
                attach_parts = util.split_path(filename)
                vendor_attachdir = os.path.join(path, attach_parts.pop(0),
                        attach_parts.pop(0), '_attachments')
                filepath = os.path.join(vendor_attachdir, *attach_parts)
            else:
                filepath = os.path.join(attachdir, filename)
            filepath = _replace_slash(filepath)
            currentdir = os.path.dirname(filepath)
            if not os.path.isdir(currentdir):
                os.makedirs(currentdir)
    
            if signatures.get(filename) != util.sign(filepath):
                resp = db.fetch_attachment(docid, filename)
                with open(filepath, 'wb') as f:
                    for chunk in resp.body_file:
                        f.write(chunk)
                logger.debug("clone attachment: %s" % filename)
                
    logger.info("%s cloned in %s" % (source, dest))
