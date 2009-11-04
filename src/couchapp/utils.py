#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# Copyright 2009 Benoit Chesneau <benoitc@e-engura.org>
#
# This software is licensed as described in the file LICENSE, which
# you should have received as part of this distribution.
#


import os
import sys
import urlparse
import urllib



__all__ = ['popen3', 'in_couchapp', 'parse_uri', 'parse_auth',
        'get_appname', 'to_bytestring', 'vendor_dir',
        'user_rcpath', 'rcpath', 'locate_program', 'deltree', 
        'relpath', 'user_path']

try:#python 2.6, use subprocess
    import subprocess
    subprocess.Popen  # trigger ImportError early
    closefds = os.name == 'posix'
    
    def popen3(cmd, mode='t', bufsize=0):
        p = subprocess.Popen(cmd, shell=True, bufsize=bufsize,
            stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE, 
            close_fds=closefds)
        p.wait()
        return (p.stdin, p.stdout, p.stderr)
except ImportError:
    subprocess = None
    popen3 = os.popen3
    
if os.name == 'nt':
    def user_rcpath():
        path = []
        try:
            home = os.path.expanduser('~')
            if sys.getwindowsversion()[3] != 2 and userdir == '~':
                 # We are on win < nt: fetch the APPDATA directory location and use
                    # the parent directory as the user home dir.
                appdir = shell.SHGetPathFromIDList(
                    shell.SHGetSpecialFolderLocation(0, shellcon.CSIDL_APPDATA))
                home = os.path.dirname(appdir)
            path.append(os.path.join(home, '.couchapp.conf'))
        except:
            home = os.path.expanduser('~')
            path.append(os.path.join(home, '.couchapp.conf'))
        userprofile = [os.environ.get('USERPROFILE')]
        if userprofile:
            path.append(os.path.join(userprofile, '.couchapp.conf'))
        return path  
    def user_path():
        path = []
        try:
            home = os.path.expanduser('~')
            if sys.getwindowsversion()[3] != 2 and userdir == '~':
                 # We are on win < nt: fetch the APPDATA directory location and use
                    # the parent directory as the user home dir.
                appdir = shell.SHGetPathFromIDList(
                    shell.SHGetSpecialFolderLocation(0, shellcon.CSIDL_APPDATA))
                home = os.path.dirname(appdir)
            path.append(os.path.join(home, '.couchapp'))
        except:
            home = os.path.expanduser('~')
            path.append(os.path.join(home, '.couchapp'))
        userprofile = [os.environ.get('USERPROFILE')]
        if userprofile:
            path.append(os.path.join(userprofile, '.couchapp'))
        return path
    
else:
    def user_rcpath():
        return [os.path.expanduser('~/.couchapp.conf')]
        
    def user_path():
        return [os.path.expanduser('~/.couchapp')]
        
        
# backport relpath from python2.6
if not hasattr(os.path, 'relpath'):
    if os.name == "nt":
        def splitunc(p):
            if p[1:2] == ':':
                return '', p # Drive letter present
            firstTwo = p[0:2]
            if firstTwo == '//' or firstTwo == '\\\\':
                # is a UNC path:
                # vvvvvvvvvvvvvvvvvvvv equivalent to drive letter
                # \\machine\mountpoint\directories...
                #           directory ^^^^^^^^^^^^^^^
                normp = os.path.normcase(p)
                index = normp.find('\\', 2)
                if index == -1:
                    ##raise RuntimeError, 'illegal UNC path: "' + p + '"'
                    return ("", p)
                index = normp.find('\\', index + 1)
                if index == -1:
                    index = len(p)
                return p[:index], p[index:]
            return '', p
            
        def relpath(path, start=os.path.curdir):
            """Return a relative version of a path"""

            if not path:
                raise ValueError("no path specified")
            start_list = os.path.abspath(start).split(os.path.sep)
            path_list = os.path.abspath(path).split(os.path.sep)
            if start_list[0].lower() != path_list[0].lower():
                unc_path, rest = splitunc(path)
                unc_start, rest = splitunc(start)
                if bool(unc_path) ^ bool(unc_start):
                    raise ValueError("Cannot mix UNC and non-UNC paths (%s and %s)"
                                                                        % (path, start))
                else:
                    raise ValueError("path is on drive %s, start on drive %s"
                                                        % (path_list[0], start_list[0]))
            # Work out how much of the filepath is shared by start and path.
            for i in range(min(len(start_list), len(path_list))):
                if start_list[i].lower() != path_list[i].lower():
                    break
            else:
                i += 1

            rel_list = [os.path.pardir] * (len(start_list)-i) + path_list[i:]
            if not rel_list:
                return os.path.curdir
            return os.path.join(*rel_list)
    else:
        def relpath(path, start=os.path.curdir):
            """Return a relative version of a path"""

            if not path:
                raise ValueError("no path specified")

            start_list = os.path.abspath(start).split(os.path.sep)
            path_list = os.path.abspath(path).split(os.path.sep)

            # Work out how much of the filepath is shared by start and path.
            i = len(os.path.commonprefix([start_list, path_list]))

            rel_list = [os.path.pardir] * (len(start_list)-i) + path_list[i:]
            if not rel_list:
                return os.path.curdir
            return os.path.join(*rel_list)
else:
    relpath = os.path.relpath 
#TODO: manage system configuration file
_rcpath = None
def rcpath():
    """ get global configuration """
    global _rcpath
    if _rcpath is None:
        if 'COUCHAPPCONF_PATH' in os.environ:
            _rcpath = []
            for p in os.environ['COUCHAPPCONF_PATH'].split(os.pathsep):
                if not p: continue
                if os.path.isdir(p):
                    for f, kind in osutil.listdir(p):
                        if f == "couchapp.conf":
                            _rcpath.append(os.path.join(p, f))
                else:
                    _rcpath.append(p)
        else:
            _rcpath = user_rcpath()
    return _rcpath
    
    
def in_couchapp():
    """ return path of couchapp if we are somewhere in a couchapp. """
    current_path = os.getcwd()
    parent = ''
    while 1:
        current_rcpath = os.path.join(current_path, '.couchapprc')
        if os.path.exists(current_rcpath):
            if current_rcpath in rcpath():
                return False
            return current_path
        parent = os.path.normpath(os.path.join(current_path, '../'))
        if parent == current_path:
            return False
        current_path = parent

def parse_uri(string):
    parts = urlparse.urlsplit(urllib.unquote(string))
    if parts[0] != 'http' and parts[0] != 'https':
        raise ValueError('Invalid dbstring')
     
    path = parts[2].strip('/').split('/')

    dbname = ''
    docid = ''
    if len(path) >= 1:
        db_parts=[]
        i = 0
        while 1:
            try:
                p = path[i]
            except IndexError:
                break

            if p == '_design': break
            db_parts.append(p)
            i = i + 1
        dbname = '/'.join(db_parts)
        
        if i < len(path) - 1:
            docid = '/'.join(path[i:])

    server_uri = '%s://%s' % (parts[0], parts[1])
    return server_uri, dbname, docid


def parse_auth(string):
    """ get username and password for an url string """
    parts = urlparse.urlsplit(urllib.unquote(string))
    
    server_parts = parts[1].split('@')
    if ":" in server_parts[0]:
        username, password = server_parts[0].split(":")
    else:
        username = server_parts[0]
        password = ''

    server_uri = "%s://%s" % (parts[0], server_parts[1])

    return username, password, server_uri

def get_appname(docid):
    """ get applicaton name for design name"""
    return docid.split('_design/')[1]


def to_bytestring(s):
    """ convert to bytestring an unicode """
    if not isinstance(s, basestring):
        return s
    if isinstance(s, unicode):
        return s.encode('utf-8')
    else:
        return s
        
# function borrowed to Fusil project(http://fusil.hachoir.org/) 
# which allowed us to use it under Apache 2 license.
def locate_program(program, use_none=False, raise_error=False):
    if os.path.isabs(program):
        # Absolute path: nothing to do
        return program
    if os.path.dirname(program):
        # ./test => $PWD/./test
        # ../python => $PWD/../python
        program = os.path.normpath(os.path.realpath(program))
        return program
    if use_none:
        default = None
    else:
        default = program
    paths = os.getenv('PATH')
    if not paths:
        if raise_error:
            raise ValueError("Unable to get PATH environment variable")
        return default
    for path in paths.split(os.pathsep):
        filename = os.path.join(path, program)
        if os.access(filename, os.X_OK):
            return filename
    if raise_error:
        raise ValueError("Unable to locate program %r in PATH" % program)
    return default
        
def deltree(path):
    for root, dirs, files in os.walk(path, topdown=False):
        for name in files:
            os.remove(os.path.join(root, name))
        for name in dirs:
            os.rmdir(os.path.join(root, name))
            
    
_vendor_dir = None
def vendor_dir():
    global _vendor_dir
    if _vendor_dir is None:
        _vendor_dir = os.path.join(os.path.dirname(__file__), 'vendor')
    return _vendor_dir
