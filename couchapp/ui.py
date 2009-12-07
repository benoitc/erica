# -*- coding: utf-8 -*-
#
# Copyright 2008,2009  Benoit Chesneau <benoitc@e-engura.org>
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

import codecs
from hashlib import md5
import logging
import os
import string
import sys

try:
    import json
except ImportError:
    import simplejson as json

from couchapp import __version__
from couchapp.extensions import GLOBAL_EXTENSIONS
import couchapp.couchdbclient as client
from couchapp.errors import AppError
from couchapp import utils

USER_AGENT = 'couchapp/%s' % __version__

class NullHandler(logging.Handler):
    """ null log handler """
    def emit(self, record):
        pass


class UI(object):
    
    DEFAULT_SERVER_URI = 'http://127.0.0.1:5984/'
    
    def __init__(self, verbose=1, logging_handler=None):
        # load user conf
        self.conf = {}
        # init extensions
        self.conf['extensions'] = GLOBAL_EXTENSIONS
        self.verbose = verbose
        self.readconfig(utils.rcpath())
        # init logger
        if logging_handler is None:
            logging_handler = NullHandler()
        self.logger = logging.getLogger("couchapp")
        self.logger.setLevel(logging.INFO)
        self.logger.addHandler(logging_handler)
        self.hooks = []
        
    def set_verbose(self, level):
        self.verbose = level
        
    def readconfig(self, fn):
        """ Get current configuration of couchapp.
        """
        if isinstance(fn, basestring):
            fn = [fn]
        
        for f in fn:
            if os.path.isfile(f):
                new_conf = self.read_json(f, use_environment=True)
                self.update_conf(new_conf)
                
    def updateconfig(self, app_dir):
        conf_files = [os.path.join(app_dir, 'couchapp.json'),
            os.path.join(app_dir, '.couchapprc')]
        self.readconfig(conf_files)
        
    def update_conf(self, new_conf):
        conf = self.conf
        for key, value in new_conf.items():
            if key in conf:
                if isinstance(value, dict):
                    conf[key].update(value)
                elif isinstance(value, list):
                    [conf[key].append(v) for v in value if v not in conf[key]]
                else:
                    conf[key]=value
            else:
                conf[key] = value
        self.conf = conf
                        
    def split_path(self, path):
        parts = []
        while True:
            head, tail = os.path.split(path)
            parts = [tail] + parts
            path = head
            if not path: break
        return parts
        
    def deltree(self, path):
        utils.deltree(path)
                
    def execute(cmd):
        return utils.popen3(cmd)
        
    def sign(self, fpath):
        """ return md5 hash from file content

        :attr fpath: string, path of file

        :return: string, md5 hexdigest
        """
        if os.path.isfile(fpath):
            m = md5()
            fp = open(fpath, 'rb')
            try:
                while 1:
                    data = fp.read(8096)
                    if not data: break
                    m.update(data)
            except IOError, msg:
                sys.stderr.write('%s: I/O error: %s\n' % (fpath, msg))
                return 1
            fp.close()
            return m.hexdigest()
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
        f.write(utils.to_bytestring(content))
        f.close()

    def write_json(self, fname, content):
        """ serialize content in json and save it

        :attr fname: string
        :attr content: string

        """
        self.write(fname, json.dumps(content).encode('utf-8'))

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
        except:
            if self.verbose >= 1:
                self.logger.error("Json is invalid, can't load %s" % fname)
            return {}
        return data
       
    def get_dbs(self, dbstring=None):
        if dbstring is None or not "/" in dbstring:
            env = self.conf.get('env', {})
            if dbstring is not None:
                db_env = "%s%s" % (self.DEFAULT_SERVER_URI, dbstring)
                if dbstring in env:
                    db_env = env[dbstring].get('db', db_env)
            else: 
                if 'default' in env:
                    db_env = env['default']['db']
                else:
                    raise AppError("database isn't specified")

            if isinstance(db_env, basestring):
                dburls = [db_env]
            else:
                dburls = db_env
        else:
            dburls = [dbstring]
        return [client.Database(self, dburl, create=True) for dburl in dburls]

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