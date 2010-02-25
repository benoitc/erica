# -*- coding: utf-8 -*-
#
# This file is part of couchapp released under the Apache 2 license. 
# See the NOTICE for more information.

__extension_name__ = "Git vendor handler"
__copyright__ = "Copyright 2008,2010  Benoit Chesneau <benoitc@e-engura.org>"
__doc__ = "couchapp vendor install|update git://somerepo.git (use git+ssh:// for ssh repos)"

from couchapp.errors import VendorError
from couchapp.utils import locate_program, popen3

def fetch(ui, url, path, *args, **opts):
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
    if ui.verbose >=2:
        ui.logger.info(child_stdout.read())
    if err:
        raise VendorError(str(err))    
    return 


scheme = ['git', 'git+ssh']