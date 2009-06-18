#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# Copyright 2009 Benoit Chesneau <benoitc@e-engura.org>
#
# This software is licensed as described in the file LICENSE, which
# you should have received as part of this distribution.
#

import os
import shutil
import tempfile

from couchapp.errors import VendorError
from couchapp.utils import locate_program, popen3, deltree

def git_clone(ui, url):
    """ return git cmd path """
    try:
        cmd = locate_program("git", raise_error=True)
    except ValueError, e:
        raise VendorError(e)
    
    # get tempdur    
    f, fname = tempfile.mkstemp()
    os.unlink(fname)
    tempdir = fname
        
    cmd += " clone --depth=1 %s %s" % (url, tempdir)
    
    # exec cmd
    (child_stdin, child_stdout, child_stderr) = popen3(cmd)
    err = child_stderr.read()
    if ui.verbose >=2:
        ui.logger.info(child_stdout.read())
    if err:
        raise VendorError(str(err))    
    return tempdir
    
    
def copy_vendor(ui, vendor_source, vendor_dir, install=False, url=None):
    level = 0
    for root, dirs, files in os.walk(vendor_source):
        dest = root.replace(vendor_source, vendor_dir)
        if not os.path.isdir(dest):
            ui.makedirs(dest)
            
        if install and level == 1:
            f = open(os.path.join(dest, ".new"), "w")
            f.write(url)
            f.close()
        
        for f in files:
            fsrc = os.path.join(root, f)
            fdest = os.path.join(dest, f)
            ui.copy(fsrc, fdest)
        
        level = level + 1
    deltree(vendor_source)
            
def install(ui, url, vendor_dir):
    vendor_source = os.path.join(git_clone(ui, url), 'vendor')
    if not os.path.isdir(vendor_source):
        return    
    copy_vendor(ui, vendor_source, vendor_dir, install=True, url=url)
          
def update(ui, url, path, vendor_dir):
    vendor_source = os.path.join(git_clone(ui, url), 'vendor')
    if not os.path.isdir(vendor_source):
        return
        
    # delete current vendor
    if ui.isdir(path):
        ui.deltree(path)
    copy_vendor(ui, vendor_source, vendor_dir)
    
cmdtable = {
    "install": install,
    "update": update
}