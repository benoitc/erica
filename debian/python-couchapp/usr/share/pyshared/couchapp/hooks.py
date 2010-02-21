# -*- coding: utf-8 -*-
#
# This file is part of couchapp released under the Apache 2 license. 
# See the NOTICE for more information.

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