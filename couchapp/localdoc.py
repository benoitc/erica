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
import os
import os.path
import re

# import json
try:
    import json
except ImportError:
    import simplejson as json
    
from couchapp.errors import *
from couchapp.macros import *
from couchapp.utils import relpath

class LocalDoc(object):
    
    def __init__(self, ui, path, create=False):
        self.ui = ui
        self.docdir = path
        self.docid = self.get_id()
        self._doc = {'_id': docid}
        
    def get_id(self):
        """
        if there is an _id file, docid is extracted from it,
        else we take the current folder name.
        """
        idfile = os.path.join(self.path, '_id')
        if os.path.exists(idfile):
            docid = self.ui.read_file(idfile)
            if docid: return docid
        return os.path.split(self.path)
        
    def __repr__(self):
        return "<%s (%s/%s)>" % (self.__class__.__name__, self.docdir, self.docid)
        
    def __str__(self):
        return json.dumps(self.doc())
        
    def create(self):
        if not os.path.isdir(self.docdir) and self.ui.verbose:
            self.ui.logger.error("%s directory doesn't exist." % self.app_dir)
            
        rcfile = os.path.join(self.docdir, '.couchapprc')
        if not os.path.isfile(rcfile):
            self.ui.write_json(rc_file, {})
        elif self.ui.verbose:
            raise AppError("CouchApp already initialized in %s." % self.app_dir)

    def push(self, dburls, noatomic=False):
        """Push a doc to a list of database `dburls`. If noatomic is true
        each attachments will be sent one by one."""
        
        for dburl in dburls:
            if noatomic:
                doc = self.doc(docid, dburl, with_attachments=False)
                save_doc(dburl, doc)
                if 'couchapp' in olddoc:
                    old_signatures = olddoc['couchapp'].get('signatures', {})
                else:
                    old_signarures = {}
                
                signatures = doc['couchapp'].get('signatures', {})
                if old_signatures:
                    for name, signature in old_signatures.items():
                        cursign = signatures.get(name)
                        if cursign is not None and cursign != signature:
                            del_attachment(dburl, self.docid, name)
                    
                for name, filepath in self.attachments():
                    if name not in old_signatures or old_signatures[name] != signatures[name]:
                        push_attachment(dburl, doc, open(filepath, r), name=name)
            else:
                doc = self.doc()
                try:
                    olddoc = get_doc(dburl, self.docid)
                    doc.update({'_rev', olddoc['_rev']})
                except ResourceNotFound:
                    pass
                save_doc(dburl, doc)   
            indexurl = self.index(dburl, doc.get('index'))
            if indexurl:
                self.ui.logger.info("Visit your CouchApp here:\n%s" % indexurl)
                        
    def doc(self, dburl=None, with_attachments=True):
        """ Function to reetrieve document object from
        document directory. If `with_attachments`is True
        attachments will be included and encoded"""
        
        manifest = []
        objects = {}
        self._doc = {'_id': self.get_id()}
        
        # get designdoc
        self._doc.update(self.dir_to_fields(self.docdir, manifest=manifest))
        
       
        if not 'couchapp' in self._doc:
             self._doc['couchapp'] = {}
            
        if self.docid.startswith('_design/'):  # process macros
            for funs in ['shows', 'lists', 'updates', 'filters']:
                if funs in self._doc:
                    package_shows(self._doc, self._doc[funs], self.docdir, objects, self.ui)
            
            if 'validate_doc_update' in self._doc:
                tmp_dict = dict(validate_doc_update=self._doc["validate_doc_update"])
                package_shows( self._doc, tmp_dict, self.docdir, objects, self.ui)
                 self._doc.update(tmp_dict)

            if 'views' in  self._doc:
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
                 self._doc['views'] = views
                 package_views(self._doc,self._doc["views"], self.docdir, objects, self.ui)
        
        signatures = {}
        attachments = {}
        for name, filepath in self.attachments():
            signatures[name] = self.ui.sign(filepath)
            if with_attachments:
                attachments[name] = {}
                f = open(filepath, rb)
                re_sp = re.compile('\s')
                attachments[name]['data'] = re_sp.sub('', base64.b64encode(f.read()))
                f.close()
        
        if with_attachments: 
            self._doc['attachments'] = attachments
            
        self._doc['couchapp'].update({
            'manifest': manifest,
            'objects': objects,
            'signatures': signatures
        })
        
        if dburl is not None:
            try:
                olddoc = get_doc(dburl, doc['_id'])
                self._doc.update({'_rev', olddoc['_rev']})
            except ResourceNotFound:
                pass
            
        return self._doc
                
    def dir_to_fields(self, current_dir='', depth=0,
                manifest=[]):
        """ process a directory and get all members """        
        
        fields={}
        if not current_dir:
            current_dir = self.docdir
        for name in os.path.listdir(current_dir):
            current_path = os.path.join(current_dir, name)
            rel_path = relpath(current_path, self.app_dir)
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
                        content = json.dumps(content)
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
        
    def _process_attachments(self, path, vendor=None):
        """ the function processing directory to yeld
        attachments. """
        if os.path.isdir(path):
            for root, dirs, files in os.walk(path):
                for dirname in dirs:
                    if dirname.startswith('.'):
                        dirs.remove(dirname)
                if files:
                    for filename in files:
                        if filename.startswith('.'):
                            continue
                        else:
                            filepath = self.ui.rjoin(root, filename) 
                            name = relpath(filepath, path)
                            if vendor is not None:
                                name = self.os.path.join('vendor', vendor, name)
                            yield (name, filepath)
                
    def attachments(self):
        """ This function yield a tupple (name, filepath) corrsponding
        to each attachments (vendor included) in the couchapp. `name`
        is the name of attachment in `_attachments` member and `filapath`
        the path to the attachment on the disk.
        
        attachments are processed later to allow us to send attachmenrs inline
        or one by one.
        """
        
        # process main attachments
        attachdir = os.path.join(self.docdir, "_attachments")
        _process_attachments(attachdir)
        vendordir = os.path.join(self.docdir, 'vendor')
        if not os.path.isdir(vendor_dir):
            if self.ui.verbose >=2:
                self.ui.logger.info("%s don't exist" % vendor_dir)
            return
            
        for name in os.path.listdir(vendor_dir):
            current_path = os.path.join(vendor_dir, name)
            if os.path.isdir(current_path):
                attachdir = os.path.join(current_path, '_attachments')
                if os.path.isdir(attach_dir):
                    _process_attachments(attach_dir, vendor=name)
    
    def index(self, dburl, index):
        if index is not None:
            return "%s/%s/%s/%s" % (db_url, '_design', self.docid, index)
        return False
        
def instance(ui, path, create):
    return LocalDoc(ui, path, create)