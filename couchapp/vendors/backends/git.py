# -*- coding: utf-8 -*-
#
# This file is part of couchapp released under the Apache 2 license. 
# See the NOTICE for more information.

import logging

from couchapp.errors import VendorError
from couchapp.util import locate_program, popen3
from couchapp.vendors.base import BackendVendor

logger = logging.getLogger(__name__)

class GitVendor(BackendVendor):
    url="http://github.com/couchapp/couchapp"
    author="Benoit Chesneau"
    author_email="benoitc@e-engura.org"
    description = "Git vendor handler"
    long_description = """couchapp vendor install|update from git::
    
    git://somerepo.git (use git+ssh:// for ssh repos)
    """
    
    scheme = ['git', 'git+ssh']

    def fetch(self, url, path, *args, **opts):
        if url.startswith("git+ssh://"):
            url = url[9:]
    
        """ return git cmd path """
        try:
            cmd = locate_program("git", raise_error=True)
        except ValueError, e:
            raise VendorError(e)
        
        cmd += " clone %s %s" % (url, path)

       # exec cmd
        (child_stdin, child_stdout, child_stderr) = popen3(cmd)
        err = child_stderr.read()
        if err:
            raise VendorError(str(err))
        logger.debug(child_stdout.read())
            
