# -*- coding: utf-8 -*-
#
# This file is part of couchapp released under the Apache 2 license. 
# See the NOTICE for more information.

import logging
import os
import shutil
import tempfile

from couchapp.errors import VendorError
from couchapp import util

logger = logging.getLogger(__name__)

def _tempdir():
    f, fname = tempfile.mkstemp()
    os.unlink(fname)
    return fname
    
class BackendVendor(object):
    """ vendor backend interface """
    url = "",
    license =  "",
    author = "",
    author_email = "",
    description = ""
    long_description = ""
    
    scheme = None
    
    def fetch(url, path, *args, **opts):
        raise NotImplementedError
    
class Vendor(object):
    """ Vendor object to manage vendors in a couchapp """
    
    def __init__(self, conf):
        """ Constructor of vendor object 
        
        :attr app_dir: string, path of app_dir
        """
        self.conf = conf
        
        # load vendor handlers
        self.scheme = self.load_vendors()

    def load_vendors(self):
        """ associate vendor  to their scheme
        Each vendor is an instance of `couchapp.vendor.base.BackendVendor`.
        
        Scheme is anything before "://" .
            
        See couchapp vendors' backends for more info.
        
        """
        scheme = {}
        for vendor_obj in self.conf.vendors:
            if not hasattr(vendor_obj, 'fetch') or \
                    not hasattr(vendor_obj, 'scheme'):
                continue
            for s in getattr(vendor_obj, 'scheme'):
                scheme[s] = vendor_obj()
        return scheme
        
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
        vendor_obj = self.find_handler(uri)
        
        # execute fetch command
        path = _tempdir()
        vendor_obj.fetch(uri, path, *args, **opts)
        
        vendors = []
        for name in os.listdir(path):
            vpath = os.path.join(path, name)
            metaf = os.path.join(vpath, "metadata.json")
            if not os.path.isfile(metaf):
                continue
            else:
                meta = util.read_json(metaf)
                meta["fetch_uri"] = uri 
                name = meta.get('name', name)
                vendors.append((name, vpath, meta))
                os.unlink(metaf)
                
        if not vendors:
            util.deltree(path)    
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
                    util.deltree(dest)
                else:
                    logger.warning("vendor: %s already installed" % name)
                    continue
            shutil.copytree(path, dest)
            util.write_json(metaf, meta)
            logger.info("%s installed in vendors" % name)
        util.deltree(temppath)
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
            meta = util.read_json(metaf)
            uri = meta.get("fetch_uri", "")
            if not uri:
                raise VendorError("Can't update vendor `%s`: fetch_uri undefined." % name)
            new_vendors, temppath = self.fetch_vendor(uri, *args, **opts)
            for vname, vpath, vmeta in new_vendors:
                if name != vname:
                    continue
                else:
                    util.deltree(dest)
                    shutil.copytree(vpath, dest)
                    util.write_json(metaf, vmeta)
                    logger.info("%s updated in vendors" % vname)
                    break

            util.deltree(temppath)
        else: # update all vendors
            updated = []
            for vendor in self.installed_vendors(vendordir):
                if vendor in updated:
                    continue
                else:
                    dest = os.path.join(vendordir, vendor)
                    metaf = os.path.join(dest, "metadata.json")
                    meta = util.read_json(metaf)
                    uri = meta.get("fetch_uri", "")
                    if not uri:
                        logger.warning(
                        "Can't update vendor `%s`: fetch_uri undefined." % vendor)
                        continue
                    else:
                        new_vendors, temppath = self.fetch_vendor(uri, *args, 
                                                                **opts)
                        for vname, vpath, vmeta in new_vendors:
                            dest1 = os.path.join(vendordir, vname)
                            metaf1 =  os.path.join(dest1, "metadata.json")
                            if os.path.exists(dest1):
                                util.deltree(dest1)
                                shutil.copytree(vpath, dest1)
                                util.write_json(metaf1, vmeta)
                                logger.info("%s updated in vendors" % vname)
                                updated.append(vname)
                            elif should_force: 
                                #install forced
                                shutil.copytree(vpath, dest1)
                                util.write_json(metaf1, vmeta)
                                logger.info("%s installed in vendors" % vname)
                                updated.append(vname)                                
                        util.deltree(temppath)
        return 0