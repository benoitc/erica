#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# Copyright 2009 Benoit Chesneau <benoitc@e-engura.org>
#
# This software is licensed as described in the file LICENSE, which
# you should have received as part of this distribution.
#

import os
import sys

from couchapp.errors import VendorError
from couchapp.utils import *

__all__ = ['VENDOR_HANDLERS', 'Vendor']

COUCHAPP_VENDOR_URL = 'git://github.com/couchapp/couchapp.git'
COUCHAPP_VENDOR_SCM = 'git'

class Vendor(object):
    """ Vendor object to manage vendors in a couchapp """
    
    def __init__(self, app_dir, ui):
        """ Constructor of vendor object 
        
        :attr app_dir: string, path of app_dir
        """
        vendor_dir = os.path.join(app_dir, "vendor")
        if not os.path.isdir(vendor_dir):
            os.makedirs(vendor_dir)
        self.vendor_dir = vendor_dir
        self.ui = ui
        self._vendor_handlers = None
        
    def global_vendor_handlers(self):
        return {
            'git': 'couchapp.vendor_handlers.git'
        }
        
    def vendor_handlers(self):
        if self._vendor_handlers is None:
            self._vendor_handlers = {}
            handlers = self.global_vendor_handlers()
            if "vendor_handlers" in self.ui.conf:
                try:
                    handlers.update(self.ui.conf['vendor_handlers'])
                except ValueError:
                    pass
                    
            for handler_name, mod_name in handlers.items():
                mod = __import__(mod_name, {}, {}, [''])
                if not hasattr(mod, 'cmdtable'):
                   continue
                cmdtable = getattr(mod, 'cmdtable')
                self._vendor_handlers[handler_name] = cmdtable

        return self._vendor_handlers
        
    def get_vendors(self):
        """ get list of vendors
        
        :return: list, vendor names
        """
        
        vendors = []
        for name in self.ui.listdir(self.vendor_dir):
            current_path = self.ui.rjoin(self.vendor_dir, name)
            if self.ui.isdir(current_path):
                vendors.append(name)
        return vendors
        
    def install(self, url, scm="git"):
        """ install a vendor in the couchapp dir.
        
        :attr url: string, url to retrieve vendor
        :attr scm: string, name of scm used to retrieve vendor. Default scm
        is git. You could add scm in ~/.couchapprc like this in `vendor_handlers`
        property :
            
        .. code-block:: javascript
            {
                "vendor_handlers": {
                    "scm": "module"
                }
            }
            
        a vendor module receive 2 actions `install` and `update` and take differents arguments:
            * install :
            
                def install(ui, url, vendor_dir):
                    ....

            * update :
            
                def update(ui, url, path, vendor_dir):
                    ....
        
        Errors should be returned to stderr. When installing the script should create a file named
        `.new` with url used to retrieve the vendor as first line.
        
        :attr verbose: boolean, False by default
        
        """
        if not scm in self.vendor_handlers():
            raise VendorError("%s scm isn't supported yet." % scm)
              
        # get list of installed vendors
        installed = self.get_vendors()
                
        handler = self.vendor_handlers()[scm]
        handler['install'](self.ui, url, self.vendor_dir)
                
        # detect new vendor application and add url so we could update later
        for name in self.ui.listdir(self.vendor_dir):
            current_path = self.ui.rjoin(self.vendor_dir, name)
            if self.ui.isdir(current_path) and name not in installed:
                new_file = self.ui.rjoin(current_path, '.new')
                if self.ui.isfile(new_file):
                    new_url = self.ui.read(new_file).strip()
                    if new_url == url:
                        mfile = self.ui.rjoin(current_path, 'metadata.json')
                        self.ui.write_json(mfile, {
                            "scm": scm,
                            "update_url": url
                        })
                        self.ui.unlink(new_file)
                        return
                        
    def _update(self, name):
        current_path = self.ui.rjoin(self.vendor_dir, name)
        if self.ui.isdir(current_path):
            mfile = self.ui.rjoin(current_path, 'metadata.json')
            metadata = self.ui.read_json(mfile)
            if not metadata and name == 'couchapp':
                update_url = COUCHAPP_VENDOR_URL
                scm = COUCHAPP_VENDOR_SCM
            elif metadata:
                update_url = metadata['update_url']
                scm = metadata['scm']
                if not scm in self.vendor_handlers():
                    scm = False
            
            if update_url and scm:
                # for now we manage only internal handlers
                handler = self.vendor_handlers()[scm]
                if self.ui.verbose >= 1:
                    self.ui.logger.info("Updating %s from %s" % (
                        current_path, update_url)) 
                handler['update'](self.ui, update_url, current_path, self.vendor_dir)         
    
    def update(self, name=None): 
        """
        update vendor or all vendors if name is None
        
        :attr name: string, name of vendor
        :attr verbose: boolean, False by default
        """
        multiple = isinstance(name, (list, tuple,))
        for vendor_name in self.ui.listdir(self.vendor_dir):
            if (multiple and vendor_name in name) or name is None:
                self._update(vendor_name)
            elif name and vendor_name == name:
                self._update(vendor_name)
                break