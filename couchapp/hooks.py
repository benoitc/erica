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

import re
try:
    import json
except ImportError:
    import couchapp.simplejson as json

from couchapp.utils import import_module, expandpath


def python_hook(ui, path, hooktype, cmd, **kwargs):
    try:
        modname, funname = cmd.split(":")
    except ValueError:
        modname = cmd
        funname = None
    
    try:
        mod = import_module(modname)
    except ImportError, e:
        if ui.verbose >= 1:
            ui.logger.error("%s: error while importing %s" % (hooktype, modname))
        return
    
    if funname is None:
        if hasattr(mod, '__call__'):
            fun = modd
        else:
            if ui.verbose >= 1:
                ui.logger.error("%s: %s isn't callable" % (hooktype, modname))
            return
    elif not hasattr(mod, funname):
        if ui.verbose >= 1:
            ui.logger.error("%s: %s don't exist in %s" % (hooktype, funname, modname))
        return
    else:
        fun = getattr(mod, funname)
        
    try:
        return fun(ui, path, hooktype, **kwargs)
    except Exception, e:
        ui.logger.error("%s:%s error while executing %s [%s]" % (hooktype, modname, cmd, str(e)))
        return -1


def external_hook(ui, path, hooktype, cmd, **kwargs):
    """
    extenal hook send a json object to the script via stdin. 
    
        {
            "path": "...",
            "hooktype": "...",
            "opts": { ... }
        }
        
    and script answer via sderr/stdout. response in stdout should be
    a json object. If response is ok, it should return {"ok": "true"}.
    """
    if os.path.isfile(cmd):
        req = json.dumps({ 
                    "path": path,
                    "hooktype": hooktype,
                    "opts": kwargs
        })
        line = "%s %s" % (cmd, req)
        
        (child_stdin, child_stdout, child_stderr) = popen3(line)
        err = child_stderr.read()
        out = child_stdout.read()
        
        if err:
            if ui.verbose >=1:
                ui.logger.error("error while running %s [%s]" % (cmd, err))
            return -1
        
        resp = None
        try:
            resp = json.loads(out)
        except ValueError:
            resp = {}
        
        if resp.get('ok', False):
            return 0
            
        if ui.verbose >=1:
            ui.logger.error("error while running %s [%s]" % (cmd, out))
        return -1    

def hook(ui, path, hooktype, **kwargs):
    if not 'hooks' in ui.conf:
        return
        
    if hooktype in ui.conf['hooks']:
        for hook in ui.conf['hooks'][hooktype]:
            if hook.startswith('python:'):
                python_hook(ui, path, hooktype, hook[7:], **kwargs)
            else:
                external_hook(ui, path, hooktype, expandpath(hook), **kwargs)