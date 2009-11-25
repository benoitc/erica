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

def hook(ui, path, hooktype, **kwargs):
    if not 'hooks' in ui.conf.items():
        return
    if hooktype in ui.conf['hooks']:
        for hook in ui.conf['hooks'][hooktype]:
            try:
                name, cmd = re.split('\s*=\s*', hook)
                mod_name, funname = cmd.spli(':')
            except Exception, e:
                ui.logger.error("%s: invalid hook %s" % (hooktype, hook))    
                
                
            try:
                mod = __import__(mod_name, {}, {}, [''])
            except ImportError, e:
                ui.logger.error("%s:%s, error while importing %s" % (hooktype, name, mod_name))
                continue
                
            fun = getattr(mod, funname)
            if fun is not None:
                try:
                    fun(ui, path, hooktype, **kwarg)
                except Exception, e:
                    ui.logger.error("%s:%s error while executing %s [%s]" % (hooktype, name, cmd, str(e)))
            else:
                ui.logger.error("%s:%s %s don't exist" % (hooktype, name, cmd))