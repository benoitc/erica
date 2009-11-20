# -*- coding: utf-8 -*-
#
# Copyright 2009 Benoit Chesneau <benoitc@e-engura.org>
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

import os
import sys

from couchapp.errors import VendorError
from couchapp.utils import *

__all__ = ['VENDOR_HANDLERS', 'Vendor']

COUCHAPP_VENDOR_URL = 'git://github.com/couchapp/couchapp.git'
COUCHAPP_VENDOR_SCM = 'git'

class Vendor(object):
    """ Vendor object to manage vendors in a couchapp """
    
    def __init__(self, appdir, ui):
        """ Constructor of vendor object 
        
        :attr app_dir: string, path of app_dir
        """
        vendordir = os.path.join(appdir, "vendor")
        if not os.path.isdir(vendordir):
            os.makedirs(vendordir)
        self.vendordir = vendordir
        self.ui = ui
        self._vendor_handlers = None
        
    def global_vendor_handlers(self):
        return {
            'git': 'couchapp.gitvendor'
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
        for name in os.path.listdir(self.vendordir):
            current_path = os.path.join(self.vendordir, name)
            if os.path.isdir(current_path):
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
        handler['install'](self.ui, url, self.vendordir)
                
        # detect new vendor application and add url so we could update later
        for name in os.path.listdir(self.vendor_dir):
            current_path = os.path.join(self.vendor_dir, name)
            if os.path.isdir(current_path) and name not in installed:
                new_file = os.path.join(current_path, '.new')
                if os.path.isfile(new_file):
                    new_url = self.ui.read(new_file).strip()
                    if new_url == url:
                        mfile = os.path.join(current_path, 'metadata.json')
                        self.ui.write_json(mfile, {
                            "scm": scm,
                            "update_url": url
                        })
                        os.unlink(new_file)
                        return
                        
    def _update(self, name):
        currentpath = os.path.join(self.vendordir, name)
        if os.path.isdir(currentpath):
            mfile = os.path.join(currentpath, 'metadata.json')
            metadata = self.ui.read_json(mfile)
            if not metadata and name == 'couchapp':
                updateurl = COUCHAPP_VENDOR_URL
                scm = COUCHAPP_VENDOR_SCM
            elif metadata:
                updateurl = metadata['update_url']
                scm = metadata['scm']
                if not scm in self.vendor_handlers():
                    scm = False
            
            if updateurl and scm:
                # for now we manage only internal handlers
                handler = self.vendor_handlers()[scm]
                if self.ui.verbose >= 1:
                    self.ui.logger.info("Updating %s from %s" % (
                        current_path, update_url)) 
                handler['update'](self.ui, updateurl, currentpath, self.vendordir)         
    
    def update(self, name=None): 
        """
        update vendor or all vendors if name is None
        
        :attr name: string, name of vendor
        :attr verbose: boolean, False by default
        """
        multiple = isinstance(name, (list, tuple,))
        for vendorname in os.path.listdir(self.vendordir):
            if (multiple and vendor_name in name) or name is None:
                self._update(vendor_name)
            elif name and vendorname == name:
                self._update(vendorname)
                break