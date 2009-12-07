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

import logging
import getopt
import os
import sys

import couchapp.commands as commands
from couchapp.errors import AppError, CommandLineError
from couchapp.extensions import get_extensions, load_extensions
from couchapp.ui import UI


def run():
    sys.exit(dispatch(sys.argv[1:]))
    
    
def dispatch(args):
    console = logging.StreamHandler()
    console.setLevel(logging.INFO)
    formatter = logging.Formatter('[%(levelname)s] %(message)s')
    console.setFormatter(formatter)
    ui = UI(logging_handler=console)
    
    try:
        return _dispatch(ui, args)
    except AppError, e:
        ui.logger.error("couchapp error: %s" % str(e))
    except KeyboardInterrupt:
        ui.logger.info("keyboard interrupt")
    except Exception, e:
        ui.logger.critical(str(e))
        raise
    return -1
    

def _dispatch(ui, args):
    # if we are in a couchapp path is not None
    path = _findcouchapp(os.getcwd())
    if path is not None:
        ui.updateconfig(path)
    
    # load extensions
    load_extensions(ui)
    # update commands
    for name, mod in get_extensions():
        cmdtable = getattr(mod, 'cmdtable', {})
        commands.table.update(cmdtable)
        
    cmd, globalopts, opts, args = _parse(ui, args)
        
    if globalopts["help"]:
        del globalopts["help"]
        return commands.usage(ui, *args, **globalopts)
    elif globalopts["version"]:
        del globalopts["version"]
        return commands.version(ui, *args, **globalopts)
    
    verbose = 1
    if globalopts["verbose"]:
        verbose = 2
    elif globalopts["quiet"]:
        verbose = 0
        
    ui.set_verbose(verbose)
    if cmd is None:
        raise CommandLineError("unknown command")

    fun = commands.table[cmd][0]
    if cmd in commands.incouchapp:
         return fun(ui, path, *args, **opts)    
    
    return fun(ui, *args, **opts)    


def _parse(ui, args):
    options = {}
    cmdoptions = {}
    try:
        args = parseopts(args, commands.globalopts, options)
    except getopt.GetoptError, e:
        raise CommandLineError(str(e))
        
    if args:
        cmd, args = args[0], args[1:]
        if cmd in commands.table:
            cmdopts = list(commands.table[cmd][1])
        else:
            cmdopts = []
    else:
        cmd = "help"
        cmdopts = list(commands.table[cmd][1])
        
    for opt in commands.globalopts:
        cmdopts.append((opt[0], opt[1], options[opt[1]], opt[3]))
        
    try:   
        args = parseopts(args, cmdopts, cmdoptions)
    except getopt.GetoptError, e:
        raise CommandLineError((cmd, e))
        
    for opt in cmdoptions.keys():
        if opt in options:
            options[opt] = cmdoptions[opt]
            del cmdoptions[opt]
        
    return cmd, options, cmdoptions, args
                        

def parseopts(args, options, state):
    namelist = []
    shortlist = ''
    argmap = {}
    defmap = {}
    
    for short, name, default, comment in options:
        oname = name
        name = name.replace('-', '_')
        argmap['-' + short] = argmap['--' + oname] = name
        defmap[name] = default
        
        if isinstance(default, list):
            state[name] = default[:]
        else:
            state[name] = default
            
        if not (default is None or default is True or default is False):
            if short: short += ':'
            if oname: oname += '='
        if short:
            shortlist += short
        if name:
            namelist.append(oname)
        
    opts, args = getopt.getopt(args, shortlist, namelist)
    for opt, val in opts:
        name = argmap[opt]
        t = type(defmap[name])
        if t is type(1):
            state[name] = int(val)
        elif t is type(''):
            state[name] = val
        elif t is type([]):
            state[name].append(val)
        elif t is type(None) or t is type(False):
            state[name] = True
        
    return args

def _findcouchapp(p):
    while not os.path.isfile(os.path.join(p, ".couchapprc")):
        oldp, p = p, os.path.dirname(p)
        if p == oldp:
            return None
    return p