# -*- coding: utf-8 -*-
#
# Copyright 2009 Benoit Chesneau <benoitc@e-engura.org>
#
# This software is licensed as described in the file LICENSE, which
# you should have received as part of this distribution.
#`

import copy
import httplib
import os
from StringIO import StringIO
import socket
import sys
import time
import urllib

import httplib2
from couchdb import Server, ResourceNotFound

from couchapp.app import Couchapp
from couchapp.utils import *

DEFAULT_SERVER_URI = 'http://127.0.0.1:5984/'
external_dir = os.path.join(os.path.dirname(__file__), '_external')

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
    """ return user conf from ~/.couchapprc 
    
    :return: dict
    """
    # this should work on windows too
    homedir = os.path.expanduser('~')
    user_conffile = os.path.join(homedir, ".couchapprc")
    if os.path.isfile(user_conffile):
        try:
            return read_json(user_conffile, use_environment=True)
        except:
            pass
    return {}


def get_config(app_dir=None):
    """ Get current configuration of couchapp. If app_dir is given
     it return user configuration and local app configuration.
     
     :attr app_dir: string, path of application
     
     :return: dict, configuratuib
    """
    conf = get_userconf()
    if app_dir and app_dir is not None:
        rc_file = os.path.join(app_dir, '.couchapprc')
        if os.path.isfile(rc_file):
            conf.update(read_json(rc_file, use_environment=True))
    return conf

class ui(object):
    
    def __init__(self, app_dir):
        self.app_dir = app_dir
        self.app = Couchapp(app_dir=self.app_dir)

        # load conf
        self.conf = get_config(app_dir)
       

    def generate_app(self, verbose=False):
        """Generates a CouchApp in app_dir"""
        ret = self.app.generate(verbose=verbose)
        if not ret['ok']:
            print >>sys.stderr, ret['error']
        self.init_app()
    
    def init_app(self, db_url=''):
        """ Initializes the .couchapprc, usually called after generate
        
        :attr db_url: string. url of default db
        """
        conf = {}
        if db_url:
            conf = { "env": { "default": { "db": db_url } } }
        
        ret = self.app.initialize(default_conf=conf)
        if not ret['ok']:
            print >>sys.stderr, ret['error']
            
    
    def get_db(self, dbstring):
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
        return db

    def push_app(self, dbstring, app_name, verbose=False, **kwargs):
        """Pushes the app specified to the CouchDB instance
        
        :attr dbstring: string, db url or db environment name.
        :attr app_name: name of app to push. 
        :attr verbose: boolean, default is False
        """
        design_doc = self.app.to_designdoc(app_name)
        
        docid = design_doc['_id']
        attach_dir = os.path.join(self.app_dir, '_attachments')
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

        for db in self.get_db(dbstring):
            if verbose >= 1:
                print "Pushing CouchApp in %s to design doc:\n%s/%s" % (self.app_dir,
                    db.resource.uri, docid)
            
            index_url = self.make_index_url(db.resource.uri, app_name, attach_dir, index)
            if index_url:
                print "Visit your CouchApp here:\n%s" % index_url

            if docid in db:
                design = db[docid]
                _app_meta = design.get('couchapp', {})

                new_doc['couchapp'] ['signatures'] = _app_meta.get('signatures', {})

                new_doc.update({
                    '_rev': design['_rev'],
                    '_attachments': design.get('_attachments', {})
                })
            
            db[docid] = new_doc
            self.send_attachments(db, design_doc, verbose=verbose)

    def _put_attachment(self, db, doc, content, filename, verbose=False):
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
                
    def send_attachments(self, db, design_doc, verbose=False):
        # init vars
        all_signatures = {}                  
        if not 'couchapp' in design_doc:
            design['couchapp'] = {}
        
        _signatures = design_doc['couchapp'].get('signatures', {})
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
            if verbose >= 2:
                print "Attaching %s" % filename
            
            f = open(value, 'rb')
            # fix issue with httplib that raises BadStatusLine
            # error because it didn't close the connection
            self._put_attachment(db, design, f, filename, verbose=verbose)
                     
        # update signatures
        design = db[docid]
        if not 'couchapp' in design:
            design['couchapp'] = {}

        all_signatures.update(_signatures)
        design['couchapp'].update({'signatures': all_signatures})
        db[docid] = design
        

    def clone_app(self, app_uri, verbose=False):
        """Clone a CouchApp from app_uri into app_dir"""
        server_uri, db_name, docid = parse_uri(app_uri) 
        couchdb_server = _server(server_uri)
        app_name = get_appname(docid)
        
        try:
            db = couchdb_server.create(db_name)
        except: # db already exist
            db = couchdb_server[db_name]
 
        rc_file = os.path.join(self.app_dir, '.couchapprc')
        if os.path.isfile(rc_file):
            print >> sys.stderr, "an app already exist here: %s" % self.app_dir
        
        try:
            design_doc = db[docid]
        except:
            if verbose >= 1:
                print >>sys.stderr, 'cant get couchapp "%s"' % app_name
                sys.exit(1)
        
        # fetch content andut them as fie handles
        attachments = {}
        if '_attachments' in design_doc:
            for filename in design_doc['_attachments'].iterkeys():
                content = StringIO(db.get_attachment(docid, filename))
                attachments[filename] = content
                
        design_doc['_attachments'] = attachments
        
        if verbose >= 1:
            print "Cloning %s to %s..." % (app_name, self.app_dir)
        ret = self.app.clone(design_doc, verbose=verbose)
     
        if not ret['ok']:
            print >>sys.stderr, ret['error']
            sys.exit(1)
            
        conf = {}
        conf['env'] = {
            'origin': {
                'db': db.resource.uri
            }
        }
        write_json(rc_file, conf)
        

    def make_index_url(self, uri, app_name, attach_dir, index):
        if index:
          return "%s/%s/%s/%s" % (uri, '_design', app_name, index)
        else:
          index_fpath = os.path.join(attach_dir, 'index.html')
          if os.path.isfile(index_fpath):
            return "%s/%s/%s/%s" % (uri, '_design', app_name, 'index.html')
          else:
            return False
