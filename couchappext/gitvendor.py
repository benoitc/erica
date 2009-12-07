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

__extension_name__ = "Git vendor handler"
__copyright__ = "Copyright 2008,2009  Benoit Chesneau <benoitc@e-engura.org>"
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