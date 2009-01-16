#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# Copyright 2008 Jan Lehnardt <jan@apache.org>
#
# This software is licensed as described in the file COPYING, which
# you should have received as part of this distribution.

"""
Script that pushes a file system heirarchy into a CouchDB design document.

Good for pure couch application development.

A port of couchapp of `couchrest` (http://github.com/jchris/couchrest/)
"""

import os, sys
from dircache import listdir
import urlparse

from md5 import md5

try:
    import simplejson as json
except ImportError:
    import json # Python 2.6
from couchdb import __version__ as VERSION, client


class InvalidApplicationNameException(Exception):
    """The current directory includes invalid characters"""


# add a depth param and skip all startswith('_') at depth==0


class Couchapp():
    """manages file hierarchies and design docs"""
    
    # def get_appname(self):
    #     """Validates the current dir name against design doc name constraints"""
    #     appname = os.curdir
    # 
    #     # TOOD: add more checking
    #     if not appname:
    #         raise InvalidApplicationNameException
    #     
    #     return appname
    
    def __init__(self, dir):
        """Initializes app object"""
        self.dir = os.path.realpath(dir)
        self.attachments = []
        self.files = {}
    
    def readfile(self, pathname):
        """Reads a file, returns a dict of content-type and content"""
        return {
            "name": pathname,
            "content-type": self.mime(pathname.split('.')[-1]), 
            "content": file(pathname).read()}
    
    def get_files(self):
        self.files = recurse_dirs(self.dir)
    
    def get_attachments(self):
        if os.path.isdir(os.path.join(self.dir, '_attachments')):
            for root, dirs, files in os.walk(os.path.join(self.dir, '_attachments')):
                for name in files:
                    path = os.path.join(root, name)
                    path = path[path.find('_attachments/')+13:]
                    self.attachments.append(path)
    
    def push(self, app_name, target):
        """Pushes dir hierarchy to a design doc"""
        
        # guess the database details
        if target.startswith('http://') or target.startswith('https://'):
            split = target.split('/')
            server_name = '/'.join(split[:3])
            db_name = '/'.join(split[3:])
        else:
            server_name = 'http://127.0.0.1:5984'
            db_name = target
        
        # open target db
        #   create if it does not exist
        server = client.Server(server_name)
        if not db_name in server:
            db = server.create(db_name)
        else:
            db = server[db_name]
        
        
        app_name = '_design/' + app_name
        
        if app_name in db:
            db[app_name] = {'_rev': db[app_name].rev}
        else:
            doc = {}
        doc
        
        # read objects recursively
        self.get_files()
        
        # get _attachments
        self.get_attachments()
        
        # calc md5s for _attachments
        self.files['signatures'] = {}
    #    for name, content in pushfiles['_attachments'].items():
    #        pushfiles['signatures'][name] = md5(content).hexdigest()
        
        
        db[app_name] = self.files
        
        for filename in self.attachments:
            pathname = os.path.join(self.dir, '_attachments', filename)
            #db.put_attachment(doc, file(pathname).read(), filename=filename)



    
def recurse_dirs(dir, depth=0):
    """Iterates through application root and populates app members"""
    entries = listdir(dir)
    files = {}
    
    for entry in entries:
        if entry.startswith('.'):
            continue
        elif entry == '_attachments':
            continue
        
        elif os.path.isdir(os.path.join(dir, entry)):
            # handle dir
            files[entry] = recurse_dirs(os.path.join(dir, entry), depth+1)
        
        else:
            # handle file
            contents = file(os.path.join(dir, entry)).read()
            if entry.endswith('.json'):
                files[entry] = json.loads(contents)
            elif entry.endswith('.js'):
                files[entry[:-3]] = contents
            else:
                files[entry] = contents
    
    return files

def main():
    # ghetto options parsing because I can't never figure out optparse
    options = dict()
    
    if len(sys.argv) <= 1:
        print 'usage: command [dir] [target] [name]'
    else:
        options['command'] = sys.argv[1]
    
    if len(sys.argv) >= 3:
        options['dir'] = sys.argv[2]
    else:
        options['dir'] = '/home/dean/Projects/couchapp/app-template' # '/home/dean/Projects/sofa'
    
    if len(sys.argv) >= 4:
        options['target'] = sys.argv[3]
    else:
        options['target'] = 'apptest'
    
    if len(sys.argv) >= 5:
        options['name'] = sys.argv[4]
    else:
        options['name'] = options['dir'].split(os.path.sep)[-1]
    
    
    app = Couchapp(options['dir'])
    if options['command'] == 'test':
        app.push('couchapp_test', 'apptest')
    elif options['command'] == 'push':
        app.push(options['name'], options['target'])
    
    # if options.command == 'generate':
    #     app.generate()

if __name__ == '__main__':
    main()