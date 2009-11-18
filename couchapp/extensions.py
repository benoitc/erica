#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# Copyright 2009 Benoit Chesneau <benoitc@e-engura.org>
#
# This software is licensed as described in the file LICENSE, which
# you should have received as part of this distribution.
#

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
            