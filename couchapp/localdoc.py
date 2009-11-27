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
import mimetypes
import os
import os.path
import re

# import json
try:
    import json
except ImportError:
    import couchapp.simplejson as json
    
from couchapp.errors import *
from couchapp.macros import *
from couchapp.utils import relpath

re_backslash = re.compile('\\\\')
def _replace_backslash(fname):
    return re_backslash.sub('/', fname)

class LocalDoc(object):
    
    def __init__(self, ui, path, create=False):
        self.ui = ui
        self.docdir = path
        self.docid = self.get_id()
        self._doc = {'_id': self.docid}
        if create: 
            self.create()
        
    def get_id(self):
        """
        if there is an _id file, docid is extracted from it,
        else we take the current folder name.
        """
        idfile = os.path.join(self.docdir, '_id')
        if os.path.exists(idfile):
            docid = self.ui.read(idfile)
            if docid: return docid
        elif os.path.exists(os.path.join(self.docdir, '.couchapprc')):
            return "_design/%s" % os.path.split(self.docdir)[1]
        
        return os.path.split(self.docdir)[1]
        
    def __repr__(self):
        return "<%s (%s/%s)>" % (self.__class__.__name__, self.docdir, self.docid)
        
    def __str__(self):
        return json.dumps(self.doc())
        
    def create(self):
        if not os.path.isdir(self.docdir) and self.ui.verbose:
            self.ui.logger.error("%s directory doesn't exist." % self.docdir)
            
        rcfile = os.path.join(self.docdir, '.couchapprc')
        if not os.path.isfile(rcfile):
            self.ui.write_json(rcfile, {})
        elif self.ui.verbose:
            raise AppError("CouchApp already initialized in %s." % self.docdir)

    def push(self, dbs, noatomic=False):
        """Push a doc to a list of database `dburls`. If noatomic is true
        each attachments will be sent one by one."""
        
        for db in dbs:
            self.olddoc = {}
            if noatomic:
                doc = self.doc(db, with_attachments=False)
                db.save_doc(doc)
                if 'couchapp' in self.olddoc:
                    old_signatures = self.olddoc['couchapp'].get('signatures', {})
                else:
                    old_signatures = {}
                
                signatures = doc['couchapp'].get('signatures', {})
                if old_signatures:
                    for name, signature in old_signatures.items():
                        cursign = signatures.get(name)
                        if cursign is not None and cursign != signature:
                            db.delete_attachment(doc, name)
               
               
                for name, filepath in self.attachments():
                    print name not in old_signatures or old_signatures.get(name) != signatures[name]
                    if name not in old_signatures or old_signatures.get(name) != signatures[name]:
                        if self.ui.verbose >= 2:
                            self.ui.logger.info("attach %s " % name)
                            
                        print doc['_id']
                        db.put_attachment(doc, open(filepath, "r"), name=name)
            else:
                doc = self.doc()
                try:
                    olddoc = db.get_doc(self.docid)
                    doc.update({'_rev': olddoc['_rev']})
                except ResourceNotFound:
                    pass
                db.save_doc(doc)   
            indexurl = self.index(db.url, doc['couchapp'].get('index'))
            if indexurl:
                self.ui.logger.info("Visit your CouchApp here:\n%s" % indexurl)
                        
    def doc(self, db=None, with_attachments=True):
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
            
                for vname, value in self._doc['views'].iteritems():
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
                if self.ui.verbose >= 2:
                    self.ui.logger.info("attach %s " % name)
                attachments[name] = {}
                f = open(filepath, "rb")
                re_sp = re.compile('\s')
                attachments[name]['data'] = re_sp.sub('', base64.b64encode(f.read()))
                f.close()
                attachments[name]['content_type'] = ';'.join(filter(None, mimetypes.guess_type(name)))
        
        if with_attachments: 
            self._doc['_attachments'] = attachments
            
        self._doc['couchapp'].update({
            'manifest': manifest,
            'objects': objects,
            'signatures': signatures
        })
        
        
        self.olddoc = {}
        if db is not None:
            try:
                self.olddoc = db.get_doc(self._doc['_id'])
                self._doc.update({'_rev': self.olddoc['_rev']})
            except ResourceNotFound:
                pass
            
        return self._doc
                
    def dir_to_fields(self, current_dir='', depth=0,
                manifest=[]):
        """ process a directory and get all members """        
        
        fields={}
        if not current_dir:
            current_dir = self.docdir
        for name in os.listdir(current_dir):
            current_path = os.path.join(current_dir, name)
            rel_path = relpath(current_path, self.docdir)
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
            elif os.path.isdir(current_path):
                manifest.append('%s/' % rel_path)
                fields[name] = self.dir_to_fields(current_path,
                        depth=depth+1, manifest=manifest)
            else:
                if self.ui.verbose >= 2:
                    self.ui.logger.info("push %s" % rel_path)
                  
                content = ''  
                if name.endswith('.json'):
                    try:
                        content = self.ui.read_json(current_path)
                    except ValueError:
                        if self.ui.verbose >= 2:
                            self.ui.logger.error("Json invalid in %s" % current_path)           
                else:
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
                            filepath = os.path.join(root, filename)
                            name = relpath(filepath, path)
                            if vendor is not None:
                                name = os.path.join('vendor', vendor, name)
                            name = _replace_backslash(name)
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
        for attachment in self._process_attachments(attachdir):
            yield attachment
        vendordir = os.path.join(self.docdir, 'vendor')
        if not os.path.isdir(vendordir):
            if self.ui.verbose >=2:
                self.ui.logger.info("%s don't exist" % vendordir)
            return
            
        for name in os.listdir(vendordir):
            current_path = os.path.join(vendordir, name)
            if os.path.isdir(current_path):
                attachdir = os.path.join(current_path, '_attachments')
                if os.path.isdir(attachdir):
                    for attachment in self._process_attachments(attachdir, vendor=name):
                        yield attachment
    
    def index(self, dburl, index):
        if index is not None:
            return "%s/%s/%s" % (dburl, self.docid, index)
        else:
            return  "%s/%s/index.html" % (dburl, self.docid)
        return False
        
def instance(ui, path, create):
    return LocalDoc(ui, path, create)