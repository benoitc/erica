# -*- coding: utf-8 -*-
#
# This file is part of couchapp released under the Apache 2 license. 
# See the NOTICE for more information.
#

import imp
import os
import re

from couchapp.utils import import_module, expandpath

_extensions = {}
_extensions_order = []

GLOBAL_EXTENSIONS = [
    "couchdbvendor=",
    "gitvendor=",
    "hgvendor="
]

def get_extensions():
    for name in _extensions_order:
        yield name, _extensions[name]

def load_path(modname, path):
    path = expandpath(path)
    if os.path.isdir(path):
        d, f = os.path.split(path.rstrip('/'))
        info = imp.find_module(f, [d])
        return imp.load_module(modname, *info)
    else:
        return imp.load_source(modname, path)

def load_extension(ui, name, path):
    if name.startswith('couchappext.'):
        sname = name[12:]
    else:
        sname = name
    
    if sname in _extensions:
        return 
        
    if path:
        mod = load_path(name, path)
    else:
        try:
            mod = import_module('.%s' % name, 'couchappext')
        except Exception:
            mod = import_module(name)
            

    _extensions[sname] = mod    
    _extensions_order.append(sname)
    
    # allow extensions to do some initial setup
    setup = getattr(mod, 'setup', False)
    if setup:
        setup(ui)

def load_extensions(ui):
    if not ui.conf:
        return 
    
    if 'extensions' in ui.conf:
        _extensions = {}
        for ext in ui.conf['extensions']:
            try:
                name, path = re.split("\s*=\s*", ext)
            except ValueError:
                if isinstance(ext, basestring):
                    name = ext
                    path = ''
                else:
                    continue
                    
            path = path.strip()
            if path and path.startswith('!'):
                continue
                
            try:
                load_extension(ui, name, path)
            except Exception, e:
                if ui.verbose >= 1:
                    ui.logger.error("failed to import %s extension: %s" % (name, str(e)))
