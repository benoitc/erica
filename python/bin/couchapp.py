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

import optparse
import dircache
import os

from posixpath import isdir, realpath
from md5 import md5

try:
    import simplejson as json
except ImportError:
    import json # Python 2.6
from couchdb import __version__ as VERSION, client

class InvalidApplicationNameException(Exception):
    """The current directory includes invalid characters"""

class Couchapp():
    """manages file hierarchies and design docs"""
    mimes = {
      "html" : "text/html",
      "htm"  : "text/html",
      "png"  : "image/png",
      "gif"  : "image/gif",
      "css"  : "text/css",
      "js"   : "test/javascript",
      "txt"  : "text/plain"
    }
    
    def mime(self, ext):
        if ext in self.mimes:
            return self.mimes[ext]
        else:
            return "unknown/" + ext
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
        self.dir = realpath(dir)

    def readfile(self, filename):
        """Reads a file, returns a dict of content-type and content"""
        return {
            "name": filename,
            "content-type": self.mime(filename.split(".")[-1]), 
            "content": file(filename).read()}

    def readdir(self, dir):
        print "reading dir " + dir;
        
        files = dict() 
        entries = dircache.listdir(dir)
        for entry in entries:
            if entry.startswith("."):
                continue

            if isdir(entry):
                # handle dir
                files[entry] = self.walk(dir + "/" + entry)
            else:
                print "reading file " + entry;
                # handle file
                files[entry] = self.readdir(dir + "/" + entry)
        return files

    def push(self, dbname):
        """pushes dir hierarchy to a design doc"""

        # if not target.startswith("http://") and 
        #    not target.startswith("https://"):
        #     target = "http://127.0.0.1:5984/" + target
        
        # open target db
        #   create if it does not exist
        server = client.Server("http://127.0.0.1:5984/")
        if not dbname in server:
            db = server.create(dbname)
        else:
            db = server[dbname]

        # create doc from dirhier
        #   grab old doc, if exists
        if  self.dir in db:
            doc = db[self.dir]
        else:
            doc = client.Document

        # read dir into a dict
        pushfiles = self.readdir(self.dir)
        pushfiles.signatures = {}
        # calc md5s for _attachments
        for attachment in pushfiles["_attachments"]:
            pushfiles.signatures[attachment.name](md5(attachment.content))
        # save or update doc
        
def main():
    usage = '%prog command [dir] [target]'

    parser = optparse.OptionParser(usage=usage, version=VERSION)

    # parser.add_option('command',
    #     action='store',
    #     dest='command',
    #     help='what command to run, see --help')

# TODO: proper arg parsing

    # options, arg = parser.parse_args()

    options = dict()
    options['command'] = "push"
    options['dir'] = "."
    options['target'] = "apptest"

#

    app = Couchapp(options['dir'])
    
    if options['command'] == "push":
        app.push(options['target'])

    # if options.command == "generate":
    #     app.generate()

if __name__ == '__main__':
    main()