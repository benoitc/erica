# -*- coding: utf-8 -*-
#
# Copyright 2008,2009  Benoit Chesneau <benoitc@e-engura.org>
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

__extension_name__ = "CouchDB vendor handler"
__copyright__ = "Copyright 2008,2009  Benoit Chesneau <benoitc@e-engura.org>"
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