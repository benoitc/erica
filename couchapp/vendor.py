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
import shutil
import tempfile

from couchapp.extensions import get_extensions
from couchapp.errors import VendorError
from couchapp.utils import *

__all__ = ['Vendor']

def _tempdir():
    f, fname = tempfile.mkstemp()
    os.unlink(fname)
    return fname


class Vendor(object):
    """ Vendor object to manage vendors in a couchapp """
    
    def __init__(self, ui):
        """ Constructor of vendor object 
        
        :attr app_dir: string, path of app_dir
        """
        self.ui = ui
        
        # load vendor handlers
        scheme, vendors = self.load_vendors()
        self.scheme = scheme
        self.vendors = vendors

    def load_vendors(self):
        """ associate vendor  to their scheme
        each vendor should contain a scheme list and a callable that should be named
        fetch. 
        Scheme is anything before "://" .
        
        ex:
        
            def fetch(ui, url, path, *args, **opts):
                ...
                
            scheme = ['git', 'git+ssh']
            
        See gitvendor module for more info.
        
        """
        scheme = {}
        vendors = []
        for name, mod in get_extensions():
            if not hasattr(mod, 'fetch') or not hasattr(mod, 'scheme'):
                continue
            else:
                vendors.append((getattr(mod, '__extension_name__', name), 
                                getattr(mod, '___copyright__', ""), 
                                getattr(mod, '__doc__', "")))
                
                fetchcmd = getattr(mod, 'fetch')
                for s in getattr(mod, 'scheme'):
                    scheme[s] = fetchcmd
        return scheme, vendors
        
    def find_handler(self, uri):
        scheme = uri.split("://")[0]
        if scheme in self.scheme:
            return self.scheme[scheme]
        else:
            raise VendorError("unkonw vendor url scheme: %s" % uri)
            
    def installed_vendors(self, vendordir):
        """ return installed vendors """
        vendors = []
        for name in os.listdir(vendordir):
            metaf = os.path.join(vendordir, name, 'metadata.json')
            if os.path.isfile(metaf):
                vendors.append(name)
            else:
                continue
        return vendors
        
    def fetch_vendor(self, uri, *args, **opts):
        """ fetch a vendor from uri """
        
        # get fetch cmd
        fetchfun = self.find_handler(uri)  
        # execute fetch command
        path = _tempdir()
        fetchfun(self.ui, uri, path, *args, **opts)
        
        vendors = []
        for name in os.listdir(path):
            vpath = os.path.join(path, name)
            metaf = os.path.join(vpath, "metadata.json")
            if not os.path.isfile(metaf):
                continue
            else:
                meta = self.ui.read_json(metaf)
                meta["fetch_uri"] = uri 
                name = meta.get('name', name)
                vendors.append((name, vpath, meta))
                os.unlink(metaf)
                
        if not vendors:
            self.ui.deltree(path)    
            raise VendorError("Invalid vendor, medata not found.")
            
        return vendors, path
        
    def install(self, appdir,  uri, *args, **opts):
        """ install a vendor in the couchapp dir.
        """
        should_force = opts.get('force', False)
        vendordir = os.path.join(appdir, "vendor")
        if not os.path.isdir(vendordir):
            os.makedirs(vendordir)
        
        new_vendors, temppath = self.fetch_vendor(uri, *args, **opts)
        for name, path, meta in new_vendors:
            dest = os.path.join(vendordir, name)
            metaf = os.path.join(dest, "metadata.json")
            if os.path.isdir(dest):
                if should_force:
                    self.ui.deltree(dest)
                    shutil.copytree(path, dest)
                    self.ui.write_json(metaf, meta)
                    if self.ui.verbose >= 1:
                        self.ui.logger.info("%s installed in vendors")
                self.ui.logger.error("vendor: %s already installed" % name)
                continue
            else:
                shutil.copytree(path, dest)
                self.ui.write_json(metaf, meta)
                if self.ui.verbose >= 1:
                    self.ui.logger.info("%s installed in vendors" % name)
        self.ui.deltree(temppath)
        return 0
    
    def update(self, appdir, name=None, *args, **opts):
        should_force = opts.get('force', False)
        vendordir = os.path.join(appdir, "vendor")
        if not os.path.isdir(vendordir):
            os.makedirs(vendordir)
            
        if name is not None:
            if name not in self.installed_vendors(vendordir):
                raise VendorError("vendor `%s` doesn't exist" % name)
            dest = os.path.join(vendordir, name)
            metaf = os.path.join(dest, "metadata.json")
            meta = self.ui.read_json(metaf)
            uri = meta.get("fetch_uri", "")
            if not uri:
                raise VendorError("Can't update vendor `%s`: fetch_uri undefined." % name)
            new_vendors, temppath = self.fetch_vendor(uri, *args, **opts)
            for vname, vpath, vmeta in new_vendors:
                if name != vname:
                    continue
                else:
                    self.ui.deltree(dest)
                    shutil.copytree(vpath, dest)
                    self.ui.write_json(metaf, vmeta)
                    if self.ui.verbose >= 1:
                        self.ui.logger.info("%s updated in vendors" % vname)
                    break

            self.ui.deltree(temppath)
        else: # update all vendors
            updated = []
            for vendor in self.installed_vendors(vendordir):
                if vendor in updated:
                    continue
                else:
                    dest = os.path.join(vendordir, vendor)
                    metaf = os.path.join(dest, "metadata.json")
                    meta = self.ui.read_json(metaf)
                    uri = meta.get("fetch_uri", "")
                    if not uri:
                        if self.ui.verbose >= 1:
                            self.ui.logget.error("Can't update vendor `%s`: fetch_uri undefined." % vendor)
                        continue
                    else:
                        new_vendors, temppath = self.fetch_vendor(uri, *args, **opts)
                        for vname, vpath, vmeta in new_vendors:
                            dest1 = os.path.join(vendordir, vname)
                            metaf1 =  os.path.join(dest1, "metadata.json")
                            if os.path.exists(dest1):
                                self.ui.deltree(dest1)
                                shutil.copytree(vpath, dest1)
                                self.ui.write_json(metaf1, vmeta)
                                if self.ui.verbose >= 1:
                                    self.ui.logger.info("%s updated in vendors" % vname)
                                updated.append(vname)
                            elif should_force: 
                                #install forced
                                shutil.copytree(vpath, dest1)
                                self.ui.write_json(metaf1, vmeta)
                                if self.ui.verbose >= 1:
                                    self.ui.logger.info("%s installed in vendors" % vname)
                                updated.append(vname)                                
                        self.ui.deltree(temppath)
        return 0