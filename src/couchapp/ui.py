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
import logging
import os
import shutil
import socket
import string
import sys
import time
import urllib

from couchapp.contrib import httplib2
from couchapp.contrib.couchdb import Server, ResourceNotFound
try:
    import json
except ImportError:
    from couchapp.contrib import simplejson as json

from couchapp.errors import AppError
from couchapp.utils import *

class NullHandler(logging.Handler):
    """ null log handler """
    def emit(self, record):
        pass


class UI(object):
    
    DEFAULT_SERVER_URI = 'http://127.0.0.1:5984/'
    
    # TODO: add possibility to load global conf
    def __init__(self, verbose=False, logging_handler=None):
        # load user conf
        self.conf = {}
        self.verbose = verbose
        self.readconfig(rcpath())
        # init logger
        if logging_handler is None:
            logging_handler = NullHandler()
        self.logger = logging.getLogger("couchapp")
        self.logger.setLevel(logging.INFO)
        self.logger.addHandler(logging_handler)
        
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
        conf_files = [os.path.join(app_dir, 'couchapp.json'),
            os.path.join(app_dir, '.couchapprc')]
        self.readconfig(conf_files)
        
    def copy_helper(self, app_dir, directory):
        """ copy helper used to generate an app"""
        template_dir = self.find_template_dir(directory)
        if template_dir:
            if directory == "vendor":
                app_dir = os.path.join(app_dir, directory)
                try:
                    os.makedirs(app_dir)
                except:
                    pass
            
            for root, dirs, files in os.walk(template_dir):
                rel = relpath(root, template_dir)
                if rel == ".":
                    rel = ""
                target_path = os.path.join(app_dir, rel)
                for d in dirs:
                    try:
                        os.makedirs(os.path.join(target_path, d))
                    except:
                        continue
                for f in files:
                    shutil.copy2(os.path.join(root, f), os.path.join(target_path, f))                
        else:
            raise AppError("Can't create a CouchApp in %s: default template not found." % (
                    app_dir))
                    
    def find_template_dir(self, directory=''):
        import couchapp
        default_locations = [
                os.path.join(couchapp.__path__[0], 'templates', directory),
                os.path.join(couchapp.__path__[0], '../../templates', directory)
        ]
        
        if directory:
            user_locations = []
            for user_location in user_path():
                user_locations.append(os.path.join(user_location, 'templates', directory))
            default_locations = user_locations + default_locations

        found = False
        for location in default_locations:
            template_dir = os.path.normpath(location)
            if os.path.isdir(template_dir):
                found = True
                break
        if found:
            return template_dir
        return False
                   
    def exists(self, path):
        return os.path.exists(path)
        
    def isfile(self, fpath):
        return os.path.isfile(fpath)
        
    def isdir(self, path):
        return os.path.isdir(path)
        
    def makedirs(self, *args):
        for a in args:
            os.makedirs(a)
            
    def listdir(self, path):
        return os.listdir(path)
        
    def walk(self, path, **kwargs):
        return os.walk(path, **kwargs)
            
    def realpath(self, path):
        return os.path.realpath(path)
        
    def dirname(self, path):
        return os.path.dirname(path)
    
    def rjoin(self, *args):
        return os.path.join(*args)

    def unlink(self, path):
        os.unlink(path)
        
    def makedirs(self, path, mode='0777'):
        os.makedirs(path, mode)
        
    def rmdir(self, path):
        os.rmdir(path)
        
    def makedirs(self, path):
        os.makedirs(path)
    
    def copy(self, src, dest):
        shutil.copy(src, dest)
        
    def relpath(self, *args):
        return relpath(*args)
    
    def split_path(self, path):
        parts = []
        while True:
            head, tail = os.path.split(path)
            parts = [tail] + parts
            path = head
            if not path: break
        return parts
        
    def deltree(self, path):
        for root, dirs, files in self.walk(path, topdown=False):
            for name in files:
                self.unlink(self.rjoin(root, name))
            for name in dirs:
                self.rmdir(self.rjoin(root, name))
                
    def copytree(self, src, dest):
        shutil.copytree(src, dest)
        
    def execute(cmd):
        return popen3(cmd)
        
    def sign(self, fpath):
        """ return md5 hash from file content

        :attr fpath: string, path of file

        :return: string, md5 hexdigest
        """
        if self.isfile(fpath):
            content = self.read(fpath, force_read=True)
            return md5(to_bytestring(content)).hexdigest()
        return ''
        
    def read(self, fname, utf8=True, force_read=False):
        """ read file content"""
        if utf8:
            try:
                f = codecs.open(fname, 'rb', "utf-8")
                data = f.read()
                f.close()
            except UnicodeError, e:
                if force_read:
                    return self.read(fname, utf8=False)
                raise
        else:
            f = open(fname, 'rb')
            data = f.read()
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
            data = self.read(fname, force_read=True)
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
        if dbname not in couchdb_server and create:
            db = couchdb_server.create(dbname)
        else:
            db = couchdb_server[dbname]
        return db
       
    def get_db(self, dbstring):
        if not dbstring or not "/" in dbstring:
            env = self.conf.get('env', {})
            if dbstring:
                db_env = "%s/%s" % (self.DEFAULT_SERVER_URI, dbstring)
                if dbstring in env:
                    db_env = env[dbstring].get('db', db_env)
            else: 
                if 'default' in env:
                    db_env = env['default']['db']
                else:
                    raise AppError("database isn't specified")

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
        
    def get_app_name(self, dbstring, default):
        env = self.conf.get('env', {})
        if dbstring and not "/" in dbstring:
            if dbstring in env:
                return env[dbstring].get('name', default)
            elif  'default' in env:
                return env['default'].get('name', default)
        elif not dbstring:
            if 'default' in env:
                return env['default'].get('name', default)
        return default

    def get_doc(self, db, docid):
        return db[docid]
        
    def save_doc(self, db, docid, doc):
        db[docid] = doc
        
    def put_attachment(self, db, doc, content, fname,
        content_length=None):
        nb_try = 0
        while True:
            error = False
            try:
                db.put_attachment(doc, content, fname, content_length=content_length)
            except (socket.error, httplib.BadStatusLine):
                time.sleep(0.4)
                error = True

            nb_try = nb_try +1
            if not error:
                break

            if nb_try > 3:
                if self.verbose >= 2:
                    self.logger.error("%s file not uploaded, sorry." % filename)
                break
        