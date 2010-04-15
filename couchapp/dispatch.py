# -*- coding: utf-8 -*-
#
# This file is part of couchapp released under the Apache 2 license. 
# See the NOTICE for more information.

import logging
import getopt
import sys

import couchapp.commands as commands
from couchapp.errors import AppError, CommandLineError
from couchapp.config import Config

logger = logging.getLogger(__name__)

class NullHandler(logging.Handler):
    """ null log handler """
    def emit(self, record):
        pass

def set_logging(level=2):
    """
    Set level of logging, and choose where to display/save logs 
    (file or standard output).
    """
    handler = logging.StreamHandler()
    logger_ = logging.getLogger('couchapp')
    logger_.setLevel(level * 10)
    format = r"%(asctime)s [%(levelname)s] %(message)s"
    datefmt = r"%Y-%m-%d %H:%M:%S"
    
    handler.setFormatter(logging.Formatter(format, datefmt))
    logger_.addHandler(handler)
    
def set_logging_level(level=2):
    logger_ = logging.getLogger('couchapp')
    logger_.setLevel(level * 10)
    

def run():
    sys.exit(dispatch(sys.argv[1:]))
    
    
def dispatch(args):
    set_logging()
    
    try:
        return _dispatch(args)
    except AppError, e:
        logger.error("couchapp error: %s" % str(e))
    except KeyboardInterrupt:
        logger.info("keyboard interrupt")
    except Exception, e:
        import traceback
        
        logger.critical("%s\n\n%s" % (str(e), traceback.format_exc()))
    return -1
    
def _dispatch(args):
    conf = Config()

    # update commands
    for mod in conf.extensions:
        cmdtable = getattr(mod, 'cmdtable', {})
        commands.table.update(cmdtable)
        
    cmd, globalopts, opts, args = _parse(args)
        
    if globalopts["help"]:
        del globalopts["help"]
        return commands.usage(conf, *args, **globalopts)
    elif globalopts["version"]:
        del globalopts["version"]
        return commands.version(conf, *args, **globalopts)
    
    verbose = 2
    if globalopts["verbose"]:
        verbose = 1
    elif globalopts["quiet"]:
        verbose = 0
        
    set_logging_level(verbose)
    if cmd is None:
        raise CommandLineError("unknown command")

    fun = commands.table[cmd][0]
    if cmd in commands.incouchapp:
         return fun(conf, conf.app_dir, *args, **opts)    
    
    return fun(conf, *args, **opts)    


def _parse(args):
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

