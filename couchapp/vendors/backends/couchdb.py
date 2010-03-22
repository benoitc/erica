# -*- coding: utf-8 -*-
#
# This file is part of couchapp released under the Apache 2 license. 
# See the NOTICE for more information.

import os

from couchapp.errors import VendorError
from couchapp.clone_app import clone
from couchapp.vendors.base import BackendVendor

class CouchdbVendor(BackendVendor):
    url="http://github.com/couchapp/couchapp"
    author="Benoit Chesneau"
    author_email="benoitc@e-engura.org"
    description = "CouchDB vendor handler"
    long_description = """couchapp vendor install|update 
couchdb://someurl/to/vendor (use couchdbs:// for https)"""

    scheme = ['couchdb', 'couchdbs']
    
    def fetch(self, url, path, *args, **opts):
        if url.startswith("couchdb://"):
            url = url.replace("couchdb://", "http://")
        else:
            url = url.replace("couchdbs://", "https://")
        
        try:
            dburl, docid = url.split('_design/')
        except ValueError:
            raise VendorError("%s isn't a valid source" % url)
        dest = os.path.join(path, docid)    
        clone(url, dest=dest)
        rcfile = os.path.join(dest, ".couchapprc")
        try:
            os.unlink(rcfile)
        except:
            pass

