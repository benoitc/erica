# -*- coding: utf-8 -*-
#
# Copyright 2008,2009 Benoit Chesneau <benoitc@e-engura.org>
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
#

import re
from couchapp.utils import import_module

_extensions = {}
_extensions_order = []

GLOBAL_EXTENSIONS = [
    "couchdb = couchapp.couchdbvendor",
    "git = couchapp.gitvendor",
    "hg = couchapp.hgvendor",
]

def get_extensions():
    for name in _extensions_order:
        yield name, _extensions[name]
    
def importp(name, mod_name):
    try:
        mod = importm(mod_name)
    except ImportError:
        mod = importm(name)
    return mod
    

def importm(path):
    if "." in path:
        i = path.rfind('.')
        pkgpath, modname = path[:i], path[i+1:]
        mod = import_module(".%s" % modname, pkgpath)
    else:
        mod = import_module(path)
    return mod
    
def load_extension(ui, name, modname):
    if name in _extensions:
        return 
        
    if modname is not None:
        try:
            mod = importm(modname)
        except ImportError:
            mod = importm(name)
    else:
        mod = importm(name)
        
    _extensions[name] = mod    
    _extensions_order.append(name)
    
    # allow to add parameters to ui
    uisetup = getattr(mod, 'uisetup', False)
    if uisetup:
        uisetup(ui)
        
    extsetup = getattr(mod, 'extsetup', False)   
    if extsetup:
        extsetup()
    
def load_extensions(ui):
    if not ui.conf:
        return 
    
    if 'extensions' in ui.conf:
        _extensions = {}
        for ext in ui.conf['extensions']:
            try:
                name, modname = re.split("\s*=\s*", ext)
            except ValueError:
                if isinstance(ext, basestring):
                    name = ext
                    modname = None
                else:
                    continue

            try:
                load_extension(ui, name, modname)
            except Exception, e:
                if ui.verbose >= 1:
                    ui.logger.error("failed to import %s extension: %s" % (name, str(e)))