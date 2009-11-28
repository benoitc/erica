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
