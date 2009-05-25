# -*- coding: utf-8 -*-
#
# Copyright 2009 Benoit Chesneau <benoitc@e-engura.org>
#
# This software is licensed as described in the file LICENSE, which
# you should have received as part of this distribution.
#`
import codecs
import copy
from hashlib import md5
import httplib
import os
import socket
import string
import sys
import time
import urllib

import httplib2
from couchdb import Server, ResourceNotFound
try:
    import json 
except ImportError:
    import simplejson as json

from couchapp.utils import *


class UI(object):
    
    DEFAULT_SERVER_URI = 'http://127.0.0.1:5984/'
    
    # TODO: add possibility to load global conf
    def __init__(self, verbose=False):
        # load user conf
        self.conf = {}
        self.verbose = verbose
        self.readconfig(rcpath())
        
    def readconfig(self, fn):
        """ Get current configuration of couchapp.

        """
        conf = self.conf or {}
        if isinstance(fn, basestring):
            fn = [fn]
        
        for f in fn:
            if self.isfile(f):
                conf.update(self.read_json(f, use_environment=True))
        self.conf = conf
        
    def updateconfig(self, app_dir):
        self.readconfig(os.path.join(app_dir, '.couchapprc'))
        
    def isfile(self, fpath):
        return os.path.isfile(fpath)
        
    def isdir(self, path):
        return os.path.isdir(path)
        
    def makedirs(self, *args):
        for a in args:
            os.makedirs(a)
            
    def listdir(self, path):
        return os.listdir(path)
        
    def walk(self, path):
        return os.walk(path)
            
    def realpath(self, path):
        return os.path.realpath(path)
        
    def dirname(self, path):
        return os.path.dirname(path)
    
    def rjoin(self, *args):
        return os.path.join(*args)

    def unlink(self, path):
        os.unlink(path)
        
    def execute(cmd):
        return popen3(cmd)
        
    def sign(self, fpath):
        """ return md5 hash from file content

        :attr fpath: string, path of file

        :return: string, md5 hexdigest
        """
        if self.isfile(fpath):
            content = self.read(fpath)
            return md5(to_bytestring(content)).hexdigest()
        return ''
        
    def read(self, fname):
        """ read file content"""
        try:
            f = codecs.open(fname, 'rb', "utf-8")
            data = f.read()
        except:
            f = open(fname, 'rb')
            data = f.read()
        finally:
            f.close()
        return data
               
    def write(self, fname, content):
        """ write content in a file

        :attr fname: string,filename
        :attr content: string
        """
        f = open(fname, 'wb')
        f.write(to_bytestring(content))
        f.close()

    def write_json(self, fname, content):
        """ serialize content in json and save it

        :attr fname: string
        :attr content: string

        """
        self.write(fname, json.dumps(content))

    def read_json(self, fname, use_environment=False):
        """ read a json file and deserialize

        :attr filename: string
        :attr use_environment: boolean, default is False. If
        True, replace environment variable by their value in file
        content

        :return: dict or list
        """
        try:
            data = self.read(fname)
        except IOError, e:
            if e[0] == 2:
                return {}
            raise

        if use_environment:
            data = string.Template(data).substitute(os.environ)

        try:
            data = json.loads(data)
        except ValueError:
            print >>sys.stderr, "Json is invalid, can't load %s" % fname
            return {}
        return data
        
    def write_err(self, *args):
        try:
            if not sys.stdout.closed: sys.stdout.flush()
            for a in args:
                sys.stderr.write(str(a))
            # stderr may be buffered under win32 when redirected to files,
            # including stdout.
            if not sys.stderr.closed: sys.stderr.flush()
        except IOError, inst:
            if inst.errno != errno.EPIPE:
                raise
                
    def write_console(self, *args):
        for a in args:
            sys.stdout.write(str(a))
        
    def server(self, server_uri):
        # init couchdb server
        if "@" in server_uri:
            http = httplib2.Http()
            username, password, server_uri = parse_auth(server_uri) 
            couchdb_server = Server(server_uri)
            http.add_credentials(username, password)
            couchdb_server.resource.http = http
        else:
            couchdb_server = Server(server_uri)
        return couchdb_server
        
    def db(self, server_uri, dbname, create=False):
        """ reurn Database object """
        couchdb_server = self.server(server_uri)
        try:
            db = couchdb_server[dbname]
        except ResourceNotFound:
            if create:
                db = couchdb_server.create(dbname)
            else:
                print >>sys.stderr, "% dont exist on %s" % (
                    dbname, server_uri)
                sys.exit(1)
        return db
       
    def get_db(self, dbstring):
        if not dbstring or not "/" in dbstring:
            env = self.conf.get('env', {})
            if dbstring:
                if dbstring in env:
                    db_env = env[dbstring]['db']
                else: 
                    db_env = "%s/%s" % (self.DEFAULT_SERVER_URI, dbstring)
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
            # create dbs if it don't exist
            _db = self.db(server_uri, db_name, create=True)
            db.append(_db)
        return db

    def get_doc(self, db, docid):
        return db[docid]
        
    def save_doc(self, db, docid, doc):
        db[docid] = doc
        
    def put_attachment(self, db, doc, content, fname):
        nb_try = 0
        while True:
            error = False
            try:
                db.put_attachment(doc, content, fname)
            except (socket.error, httplib.BadStatusLine):
                time.sleep(0.4)
                error = True

            nb_try = nb_try +1
            if not error:
                break

            if nb_try > 3:
                if self.verbose >= 2:
                    print >>sys.stderr, "%s file not uploaded, sorry." % filename
                break
                
    
        