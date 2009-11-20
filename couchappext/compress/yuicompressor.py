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


"""
simple backend to use yuicompressor to compress files
"""
__about__ = "yui compressor v2.4.1"

import codecs
import os
from popen2 import popen2
import tempfile

from couchapp.utils import read_file



def compress(js):
    cmd_path = os.path.join(os.path.dirname(__file__), 
            'yuicompressor-2.4.1.jar')
    fd, fname = tempfile.mkstemp()
    f = codecs.getwriter('utf8')(os.fdopen(fd, "w"))
    f.write(js)
    f.close()
    cmd = "java -jar %s --type js %s" % (cmd_path, fname) 
    sout, sin = popen2(cmd)
    data = sout.read()
    os.unlink(fname) 
    return data
