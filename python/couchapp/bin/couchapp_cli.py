#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# Copyright 2008 Jan Lehnardt <jan@apache.org>
# Copyright 2009 Benoit Chesneau <benoitc@e-engura.org>
#
# This software is licensed as described in the file LICENSE, which
# you should have received as part of this distribution.

"""
Script that pushes a file system hierarchy into a CouchDB design document.

Good for pure Couch application development.

A port of couchapp from Ruby (http://github.com/jchris/couchapp)
"""

import os
import sys
from optparse import OptionParser, OptionGroup


import couchapp
from couchapp.utils import in_couchapp


def generate(appname):
    appdir = os.path.normpath(os.path.join(os.getcwd(), appname))
    print "Generating a new CouchApp in %s" % appdir
    couchapp.FileManager.generate_app(appdir)

def init(appdir, dburl):
    print "Init a new CouchApp in %s" % appdir
    couchapp.FileManager.init(appdir, dburl)

def push(appdir, appname, dbstring):
    try:
        fm = couchapp.FileManager(dbstring, appdir)
    except ValueError, e:
        print>>sys.stderr, e
        return  
    fm.push_app(appdir, appname,verbose=True)

def clone(app_uri, app_dir):
    couchapp.FileManager.clone(app_uri, app_dir)

def main():
    parser = OptionParser(usage='%prog [options] cmd', version="%prog " + couchapp.__VERSION__)
    group = OptionGroup(parser, "init", "couchapp init [options] [appdir]")
    group.add_option("--db", action="store", help="full uri of default database")
    parser.add_option_group(group)

    options, args = parser.parse_args()

    if len(args) < 1:
        return parser.error('incorrect number of arguments')

    if args[0] == 'generate':
        if len(args) < 2:
            return parser.error('cmd: "generate appname"'+
                    '\n\nIncorrect number of arguments, appname is'+
                    ' missing')
        appname = args[1]
        generate(appname)
    elif args[0] == 'push':
        appname = ''
        rel_path = '.'
        dbstring = ''
        if len(args) == 4:
            if in_couchapp():
                return parser.error('incorrect number of arguments')
            rel_path = args[1]
            appname = args[2]
            dbstring = args[3]
        elif len(args) == 3:
            rel_path = in_couchapp()
            if rel_path:
                if args[1] != '.':
                    appname = args[1]
                dbstring = args[2]
            else:
                rel_path = args[1]
                dbstring = args[2]
        elif len(args) == 2:
            rel_path = in_couchapp()
            if rel_path:
                if args[1] != '.':
                    dbstring = args[1]
            else:
                rel_path = args[1]
        elif len(args) == 1:
            rel_path = in_couchapp()
            if not rel_path:
                rel_path = '.'

        appdir = os.path.normpath(os.path.join(os.getcwd(), rel_path))
        if not appname: 
            appname = ''.join(appdir.split('/')[-1:])
        push(appdir, appname, dbstring)
    elif args[0] == 'clone' or args[0] == 'pull':
        if len(args) < 2:
            return parser.error('incorrect number of arguments')
        if len(args) == 3:
            app_dir = args[2]
        else:
            app_dir = ''
        clone(args[1], app_dir)
    
    elif args[0] == 'init':
        dburl = options.db or ''
        try:
            appdir = args[1]
        except IndexError:
            appdir = '.'
        init(appdir, dburl)
    else:
        print "%s is unknown" % args[0]

if __name__ == '__main__':
    main()
