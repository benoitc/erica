# -*- coding: utf-8 -*-
#
# This file is part of couchapp released under the Apache 2 license. 
# See the NOTICE for more information.

from couchapp.vendors.base import Vendor

def vendor_install(conf, dest, source, *args, **opts):
    vendor = Vendor(conf)
    vendor.install(dest, source, *args, **opts)
    
def vendor_update(conf, dest, name=None, *args, **opts):
    vendor = Vendor(conf)
    vendor.update(dest, name, *args, **opts)