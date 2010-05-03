# -*- coding: utf-8 -*-
#
# This file is part of couchapp released under the Apache 2 license. 
# See the NOTICE for more information.

from __future__ import with_statement

import codecs
from hashlib import md5
import logging
import os
import pkg_resources
import string
import sys

try:
    import simplejson as json
except ImportError:
    import couchapp.simplejson as json

from couchapp.errors import ScriptError
        
logger = logging.getLogger(__name__)

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
    from win32com.shell import shell, shellcon
    def user_rcpath():
        path = []
        try:
            home = os.path.expanduser('~')
            if sys.getwindowsversion()[3] != 2 and home == '~':
                 # We are on win < nt: fetch the APPDATA directory location and use
                    # the parent directory as the user home dir.
                appdir = shell.SHGetPathFromIDList(
                    shell.SHGetSpecialFolderLocation(0, shellcon.CSIDL_APPDATA))
                home = os.path.dirname(appdir)
            path.append(os.path.join(home, '.couchapp.conf'))
        except:
            home = os.path.expanduser('~')
            path.append(os.path.join(home, '.couchapp.conf'))
        userprofile = os.environ.get('USERPROFILE')
        if userprofile:
            path.append(os.path.join(userprofile, '.couchapp.conf'))
        return path  
    def user_path():
        path = []
        try:
            home = os.path.expanduser('~')
            if sys.getwindowsversion()[3] != 2 and home == '~':
                 # We are on win < nt: fetch the APPDATA directory location and use
                    # the parent directory as the user home dir.
                appdir = shell.SHGetPathFromIDList(
                    shell.SHGetSpecialFolderLocation(0, shellcon.CSIDL_APPDATA))
                home = os.path.dirname(appdir)
            path.append(os.path.join(home, '.couchapp'))
        except:
            home = os.path.expanduser('~')
            path.append(os.path.join(home, '.couchapp'))
        userprofile = os.environ.get('USERPROFILE')
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
                    for f, kind in os.listdir(p):
                        if f == "couchapp.conf":
                            _rcpath.append(os.path.join(p, f))
                else:
                    _rcpath.append(p)
        else:
            _rcpath = user_rcpath()
    return _rcpath
    

def findcouchapp(p):
    while not os.path.isfile(os.path.join(p, ".couchapprc")):
        oldp, p = p, os.path.dirname(p)
        if p == oldp:
            return None
    return p

   
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
            os.unlink(os.path.join(root, name))
        for name in dirs:
            os.rmdir(os.path.join(root, name))
    try:
        os.rmdir(path)
    except:
        pass

def split_path(path):
    parts = []
    while True:
        head, tail = os.path.split(path)
        parts = [tail] + parts
        path = head
        if not path: break
    return parts
    
def sign(fpath):
    """ return md5 hash from file content

    :attr fpath: string, path of file

    :return: string, md5 hexdigest
    """
    if os.path.isfile(fpath):
        m = md5()
        with  open(fpath, 'rb') as fp:
            try:
                while 1:
                    data = fp.read(8096)
                    if not data: break
                    m.update(data)
            except IOError, msg:
                logger.error('%s: I/O error: %s\n' % (fpath, msg))
                return 1
            return m.hexdigest()
    return ''
    
def read(fname, utf8=True, force_read=False):
    """ read file content"""
    if utf8:
        try:
            with codecs.open(fname, 'rb', "utf-8") as f:
                return f.read()
        except UnicodeError, e:
            if force_read:
                return read(fname, utf8=False)
            raise
    else:
        with open(fname, 'rb') as f:
            return f.read()
           
def write(fname, content):
    """ write content in a file

    :attr fname: string,filename
    :attr content: string
    """
    with open(fname, 'wb') as f:
        f.write(to_bytestring(content))

def write_json(fname, content):
    """ serialize content in json and save it

    :attr fname: string
    :attr content: string

    """
    write(fname, json.dumps(content).encode('utf-8'))

def read_json(fname, use_environment=False):
    """ read a json file and deserialize

    :attr filename: string
    :attr use_environment: boolean, default is False. If
    True, replace environment variable by their value in file
    content

    :return: dict or list
    """
    try:
        data = read(fname, force_read=True)
    except IOError, e:
        if e[0] == 2:
            return {}
        raise

    if use_environment:
        data = string.Template(data).substitute(os.environ)

    try:
        data = json.loads(data)
    except ValueError:
        logger.error("Json is invalid, can't load %s" % fname)
        return {}
    return data            
    
_vendor_dir = None
def vendor_dir():
    global _vendor_dir
    if _vendor_dir is None:
        _vendor_dir = os.path.join(os.path.dirname(__file__), 'vendor')
    return _vendor_dir


def expandpath(path):
    return os.path.expanduser(os.path.expandvars(path))



class ShellScript(object):
    """ simple object used to manage extensions or hooks from external
    scripts in any languages """
    
    def __init__(self, cmd):
        self.cmd = cmd
        
    def __call__(self, *args, **options):
        cmd = self.cmd + " "
        
        # cmd += " ".join(
        #     ["--%s=%s" % (k, v) for k, v in options.items()] +
        #     list(args)
        # )
        (child_stdin, child_stdout, child_stderr) = popen3(cmd)
        err = child_stderr.read()
        if err:
            raise ScriptError(str(err))
        return (child_stdout.read())
        

def parse_uri(uri, section):
    if uri.startswith("egg:"):
        # uses entry points
        entry_str = uri.split("egg:")[1]
        try:
            dist, name = entry_str.rsplit("#",1)
        except ValueError:
            dist = entry_str
            name = "main"

        return pkg_resources.load_entry_point(dist, section, name)
    elif uri.startswith("python:"):
        uri1 = uri.split("python:")[1]
        components = uri1.split('.')
        if len(components) == 1:
            raise RuntimeError("extension uri invalid")
        klass = components.pop(-1)
        mod = __import__('.'.join(components))
        for comp in components[1:]:
            mod = getattr(mod, comp)
        return getattr(mod, klass)
    else:
        raise RuntimeError("extension uri invalid")
        
def parse_hooks_uri(uri):
    if uri.startswith("python:") or uri.startswith("egg:"):
        return parse_uri(uri, "couchapp.hook")
    return ShellScript(uri)
