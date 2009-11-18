#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# Copyright 2009 Benoit Chesneau <benoitc@e-engura.org>
#
# This software is licensed as described in the file LICENSE, which
# you should have received as part of this distribution.
#


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
