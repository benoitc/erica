# -*- coding: utf-8 -*-
#
# This file is part of couchapp released under the Apache 2 license. 
# See the NOTICE for more information.


"""
simple backend to use yuicompressor to compress files
"""
__about__ = "yui compressor v2.4.1"


import codecs
import os
from popen2 import popen2
import tempfile


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
