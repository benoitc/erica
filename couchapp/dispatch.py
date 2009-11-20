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
from optparse import OptionParser, OptionGroup
import sys

import couchapp
import couchapp.commands as commands
from couchapp.ui import UI


def run():
    exit(dispatch())
    
    
def dispatch():
    ui, opts, args = _parse()

    try:
        _dispatch(ui, opts, args)
    except AppError, e:
        ui.logger.error("couchapp error: %s" % str(e))
    except KeyboardInterrupt:
        ui.logger.info("keyboard interrupt")
    except Exception, e:
        ui.logger.critical(str(e))
    
    return -1
    

def _dispatch(ui, opts, args):
    if len(args) < 1:
        return parser.error('incorrect number of arguments')
        
    cmd = args.pop(0)
    if cmd not in commands.table:
        return parser.error('unknown command')
    
    fun = commands.table[cmd][0]
    if cmd in commands.incouchapp:
        path = _findrepo(os.getcwd()) or ""
        if not path:
            if cmd in commands.withcmd:
                if len(args) < 2:
                    return parser.error('incorrect number of arguments')
                subcmd = args.pop(0)
                dest = args.pop(0)
                fun(subcmd, dest, *args, **opts)
            else:
                dest = args.pop(0)
                fun(dest, *args, **opts)
        elif cmd in commands.withcmd:
            subcmd = args.pop[0]
            fun(subcmd, path, *args, **opts)
        else:
            fun(path, *args, **opts)
    else:
        fun(*args, **opts)
        

def _parse():
    console = logging.StreamHandler()
    console.setLevel(logging.INFO)
    formatter = logging.Formatter('[%(levelname)s] %(message)s')
    console.setFormatter(formatter)
    
    parser = OptionParser(usage="usage: %prog [options] [cmd] arg1...argn",
                          version="%prog " + couchapp.__version__,
                          conflict_handler="resolve")
    for option, dest, action, default, const, help in commands.globalopts:
        if action == "store_const":
            parser.add_option(option, dest=dest, default=default,  
                        action=action, const=const, help=help)
        else:
            parser.add_option(option, dest=dest, default=default,  
                        action=action, help=help)

    for cmd, v in commands.table.items():
        group = OptionGroup(parser, cmd, v[2])
        print v[1]
        for opt in v[1]:
            print opt
            option, dest, action, default, const, help = opt
            if action == "store_const":
                group.add_option(option, dest=dest, default=default,  
                                action=action, const=const, help=help)
            else:
                group.add_option(option, dest=dest, default=default,  
                                action=action, help=help)
        parser.add_option_group(group)
    opts, args = parser.parse_args()
    ui = UI(verbose=opts.verbose, logging_handler=console)
    return ui, opts, args
                        

def _findcouchapp(p):
    while not os.path.isdir(os.path.join(p, ".couchapprc")):
        oldp, p = p, os.path.dirname(p)
        if p == oldp:
            return None

    return p