# -*- coding: utf-8 -*-
#
# This file is part of couchapp released under the Apache 2 license. 
# See the NOTICE for more information.

__extension_name__ = "CouchDB vendor handler"
__copyright__ = "Copyright 2008,2010  Benoit Chesneau <benoitc@e-engura.org>"
__doc__ = "couchapp vendor install|update couchdb://someurl/to/vendor (use couchdbs:// for https)"


import os

from couchapp.errors import VendorError
from couchapp.app import clone


def fetch(ui, url, path, *args, **opts):
    if url.startswith("couchdb://"):
        url = url.replace("couchdb://", "http://")
    else:
        url = url.replace("couchdbs://", "https://")
        
    try:
        dburl, docid = url.split('_design/')
    except ValueError:
        raise VendorError("%s isn't a valid source" % url)
    dest = os.path.join(path, docid)    
    clone(ui, url, dest=dest)
    rcfile = os.path.join(dest, ".couchapprc")
    try:
        os.unlink(rcfile)
    except:
        pass

scheme = ['couchdb', 'couchdbs']