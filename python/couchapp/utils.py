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
        'get_appname', 'to_bytestring', 'external_dir', 'vendor_dir',
        'user_rcpath', 'rcpath']


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
        try:
            home = os.path.expanduser('~')
            if sys.getwindowsversion()[3] != 2 and userdir == '~':
                 # We are on win < nt: fetch the APPDATA directory location and use
                    # the parent directory as the user home dir.
                appdir = shell.SHGetPathFromIDList(
                    shell.SHGetSpecialFolderLocation(0, shellcon.CSIDL_APPDATA))
                home = os.path.dirname(appdir)
            path = os.path.join(home, '.couchapprc')
        except:
            home = os.path.expanduser('~')
            path = os.path.join(home, '.couchapprc')
        userprofile = [os.environ.get('USERPROFILE')]
        if userprofile:
            path.append(os.path.join(userprofile, '.couchapprc'))
        return path
else:
    def user_rcpath():
        return [os.path.expanduser('~/.couchapprc')]
        
        
#TODO: manage system configuration file
_rcpath = None
def rcpath():
    """ get global configuration """
    global _rcpath
    if _rcpath is None:
        if 'COUCHAPPRC_PATH' in os.environ:
            _rcpath = []
            for p in os.environ['COUCHAPPRC_PATH'].split(os.pathsep):
                if not p: continue
                if os.path.isdir(p):
                    for f, kind in osutil.listdir(p):
                        if f.endswith('.rc'):
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
        
_external_dir = None
def external_dir():
    global _external_dir
    if _external_dir is None:
        _external_dir = os.path.join(os.path.dirname(__file__), '_external')
    return _external_dir
    
_vendor_dir = None
def vendor_dir():
    global _vendor_dir
    if _vendor_dir is None:
        _vendor_dir = os.path.join(external_dir(), 'vendor')
    return _vendor_dir
