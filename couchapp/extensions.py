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

class Extensions(object):
    
    __state__ = {
        "_hooks": {},
    }
    
    def __init__(self, app):
        self.__dict__ = self.__state__
        self.app = app
        
    def load(self):
        if 'extensions' in self.app.ui.conf:
            for name, options in self.app.ui.conf['extensions'].items():
                if 'ext' in options:
                    mod_name = options['ext']
                else:
                    mod_name = name
                
                mod = None
                try:
                    mod = __import__(mod_name, {}, {}, [''])
                except ImportError:
                    mod_name = "couchapp.couchapp_ext.%s" % mod_name
                    try:
                        mod = __import__(mod_name, {}, {}, [''])
                    except ImportError, e:
                        if self.app.ui.verbose:
                            self.app.ui.logger.info("%s extension can't be loaded (%s)" % (name, str(e)))
                        mod = None
                    
                if mod is not None and hasattr(mod, 'hook'):
                    self._hooks[name] = getattr(mod, 'hook')
    
    def notify(self, hooktype, ui, app_dir, **kwargs):
        for fun in self._hooks.values():
            try:
                fun(ui, app_dir, hooktype, **kwargs)
            except Exception, e:
                self.app.ui.logger.warning(unicode(e))
                continue
            