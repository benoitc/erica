# -*- coding: utf-8 -*-
#
# This file is part of couchapp released under the Apache 2 license. 
# See the NOTICE for more information.


from __future__ import with_statement
import base64
import logging
import mimetypes
import os
import os.path
import re
import webbrowser
# import json
try:
    import json
except ImportError:
    import couchapp.simplejson as json

from couchapp.errors import ResourceNotFound
from couchapp.macros import package_shows, package_views
from couchapp import util

if os.name == 'nt':
    def _replace_backslash(name):
        return name.replace("\\", "/")
else:
    def _replace_backslash(name):
        return name
        
logger = logging.getLogger(__name__)

class LocalDoc(object):
    
    def __init__(self, path, create=False, docid=None):
        self.docdir = path
        self.ignores = []
        ignorefile = os.path.join(path, '.couchappignore')
        if os.path.exists(ignorefile):
            # A .couchappignore file is a json file containing a
            # list of regexps for things to skip
            self.ignores = json.load(open(ignorefile, 'r'))
        if not docid:
            docid = self.get_id()
        self.docid = docid
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
            docid = util.read(idfile).split("\n")[0].strip()
            if docid: return docid
        return "_design/%s" % os.path.split(self.docdir)[1]
        
    def __repr__(self):
        return "<%s (%s/%s)>" % (self.__class__.__name__, self.docdir, self.docid)
        
    def __str__(self):
        return json.dumps(self.doc())
        
    def create(self):
        if not os.path.isdir(self.docdir):
            logger.error("%s directory doesn't exist." % self.docdir)
            
        rcfile = os.path.join(self.docdir, '.couchapprc')
        if not os.path.isfile(rcfile):
            util.write_json(rcfile, {})
        else:
            logger.info("CouchApp already initialized in %s." % self.docdir)

    def push(self, dbs, noatomic=False, browser=False, force=False):
        """Push a doc to a list of database `dburls`. If noatomic is true
        each attachments will be sent one by one."""
        for db in dbs:
            if noatomic:
                doc = self.doc(db, with_attachments=False)
                try:
                    self.olddoc = db.open_doc(doc['_id'])
                    doc['_attachments'] = self.olddoc.get('_attachments')
                except ResourceNotFound:
                    self.olddoc = {}
                
                db.save_doc(doc, force_update=True)
                if 'couchapp' in self.olddoc:
                    old_signatures = self.olddoc['couchapp'].get('signatures', 
                                                                {})
                else:
                    old_signatures = {}
                
                signatures = doc['couchapp'].get('signatures', {})

                if old_signatures:
                    for name, signature in old_signatures.items():
                        cursign = signatures.get(name)
                        if not cursign:
                            db.delete_attachment(doc, name)
                        elif cursign != signature:
                            db.delete_attachment(doc, name)
                        else:
                            continue
                            
                for name, filepath in self.attachments():
                    if old_signatures.get(name) != signatures.get(name) or force:
                        logger.debug("attach %s " % name)
                        db.put_attachment(doc, open(filepath, "r"), 
                                            name=name)
            else:
                doc = self.doc()
                db.save_doc(doc, force_update=True)
            indexurl = self.index(db.uri, doc['couchapp'].get('index'))
            if indexurl:
                logger.info("Visit your CouchApp here:\n%s" % indexurl)
                if browser:
                    webbrowser.open_new_tab(indexurl)            
                        
    def doc(self, db=None, with_attachments=True):
        """ Function to reetrieve document object from
        document directory. If `with_attachments` is True
        attachments will be included and encoded"""
        
        manifest = []
        objects = {}
        self._doc = {'_id': self.docid}
        
        # get designdoc
        self._doc.update(self.dir_to_fields(self.docdir, manifest=manifest))
        
       
        if not 'couchapp' in self._doc:
             self._doc['couchapp'] = {}
            
        signatures = {}
        attachments = {}
        for name, filepath in self.attachments():
            signatures[name] = util.sign(filepath)
            if with_attachments:
                logger.debug("attach %s " % name)
                attachments[name] = {}
                with open(filepath, "rb") as f:
                    re_sp = re.compile('\s')
                    attachments[name]['data'] = re_sp.sub('', 
                                        base64.b64encode(f.read()))
                attachments[name]['content_type'] = ';'.join(filter(None, 
                                            mimetypes.guess_type(name)))
        
        if with_attachments: 
            self._doc['_attachments'] = attachments
            
        self._doc['couchapp'].update({
            'manifest': manifest,
            'objects': objects,
            'signatures': signatures
        })
        
        
        if self.docid.startswith('_design/'):  # process macros
            for funs in ['shows', 'lists', 'updates', 'filters', 
                    'fulltext']:
                if funs in self._doc:
                    package_shows(self._doc, self._doc[funs], self.docdir, 
                            objects)
            
            if 'validate_doc_update' in self._doc:
                tmp_dict = dict(validate_doc_update=self._doc[
                                                    "validate_doc_update"])
                package_shows( self._doc, tmp_dict, self.docdir, 
                    objects)
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
                package_views(self._doc,self._doc["views"], self.docdir, 
                        objects)
        
        self.olddoc = {}
        if db is not None:
            try:
                self.olddoc = db.open_doc(self._doc['_id'])
                self._doc.update({'_rev': self.olddoc['_rev']})
            except ResourceNotFound:
                pass
            
        return self._doc
    
    def check_ignore(self, item):
        for i in self.ignores:
            match = re.match(i, item)
            if match:
                logger.debug("ignoring %s" % item)
                return True
        return False
    
    def dir_to_fields(self, current_dir='', depth=0,
                manifest=[]):
        """ process a directory and get all members """        
        
        fields={}
        if not current_dir:
            current_dir = self.docdir
        for name in os.listdir(current_dir):
            current_path = os.path.join(current_dir, name)
            rel_path = _replace_backslash(util.relpath(current_path, self.docdir))
            if name.startswith("."):
                continue
            elif self.check_ignore(name):
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
                    content = util.read_json(current_path)
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
                logger.debug("push %s" % rel_path)
                  
                content = ''  
                if name.endswith('.json'):
                    try:
                        content = util.read_json(current_path)
                    except ValueError:
                        logger.error("Json invalid in %s" % current_path)           
                else:
                    try:
                        content = util.read(current_path)
                    except UnicodeDecodeError, e:
                        logger.warning("%s isn't encoded in utf8" % current_path)
                        content = util.read(current_path, utf8=False)
                        try:
                            content.encode('utf-8')
                        except UnicodeError, e:
                            logger.warning(
                            "plan B didn't work, %s is a binary" % current_path)
                            logger.warning("use plan C: encode to base64")   
                            content = "base64-encoded;%s" % base64.b64encode(
                                                                        content)

                
                # remove extension
                name, ext = os.path.splitext(name)
                if name in fields and ext in ('.txt'):
                    logger.warning(
        "%(name)s is already in properties. Can't add (%(name)s%(ext)s)" % {
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
                    elif self.check_ignore(dirname):
                        dirs.remove(dirname)
                if files:
                    for filename in files:
                        if filename.startswith('.'):
                            continue
                        elif self.check_ignore(filename):
                            continue
                        else:
                            filepath = os.path.join(root, filename)
                            name = util.relpath(filepath, path)
                            if vendor is not None:
                                name = os.path.join('vendor', vendor, name)
                            name = _replace_backslash(name)
                            yield (name, filepath)
                
    def attachments(self):
        """ This function yield a tuple (name, filepath) corresponding
        to each attachment (vendor included) in the couchapp. `name`
        is the name of attachment in `_attachments` member and `filepath`
        the path to the attachment on the disk.
        
        attachments are processed later to allow us to send attachments inline
        or one by one.
        """
        # process main attachments
        attachdir = os.path.join(self.docdir, "_attachments")
        for attachment in self._process_attachments(attachdir):
            yield attachment
        vendordir = os.path.join(self.docdir, 'vendor')
        if not os.path.isdir(vendordir):
            logger.debug("%s don't exist" % vendordir)
            return
            
        for name in os.listdir(vendordir):
            current_path = os.path.join(vendordir, name)
            if os.path.isdir(current_path):
                attachdir = os.path.join(current_path, '_attachments')
                if os.path.isdir(attachdir):
                    for attachment in self._process_attachments(attachdir, 
                                                        vendor=name):
                        yield attachment
    
    def index(self, dburl, index):
        if index is not None:
            return "%s/%s/%s" % (dburl, self.docid, index)
        else:
            return  "%s/%s/index.html" % (dburl, self.docid)
        return False
        
def document(path, create=False, docid=None):
    return LocalDoc(path, create=create, docid=docid)
