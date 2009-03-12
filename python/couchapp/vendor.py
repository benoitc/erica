#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# Copyright 2009 Benoit Chesneau <benoitc@e-engura.org>
#
# This software is licensed as described in the file LICENSE, which
# you should have received as part of this distribution.
#

import os
import sys

from couchapp.config import get_userconf, external_dir
from couchapp.utils import read_json, write_json, read_file, _popen3

__all__ = ['VENDOR_HANDLERS', 'Vendor', 'external_vendor_dir']

COUCHAPP_VENDOR_URL = 'git://github.com/jchris/couchapp.git'
COUCHAPP_VENDOR_SCM = 'git'

external_vendor_dir = os.path.join(external_dir, 'vendor')
VENDOR_HANDLERS = {
    'git': os.path.join(external_vendor_dir, 'git.sh')
}


def _vendor_handlers():
    user_conf = get_userconf()
    vendor_handlers = VENDOR_HANDLERS
    if "vendor_handlers" in user_conf:
        try:
            vendor_handler.update(user_conf['vendor_handlers'])
        except ValueError:
            pass
    return vendor_handlers
    

class Vendor(object):
    """ Vendor object to manage vendors in a couchapp """
    
    def __init__(self, app_dir):
        """ Constructor of vendor object 
        
        :attr app_dir: string, path of app_dir
        """
        vendor_dir = os.path.join(app_dir, "vendor")
        if not os.path.isdir(vendor_dir):
            os.makedirs(vendor_dir)
        self.vendor_dir = vendor_dir
        
        self.vendor_handlers = _vendor_handlers()
        
    def get_vendors(self):
        """ get list of vendors
        
        :return: list, vendor names
        """
        
        vendors = []
        for name in os.listdir(self.vendor_dir):
            current_path = os.path.join(vendor_dir, name)
            if os.path.isdir(current_path):
                vendors.append(name)
        return vendors
        
    def install(self, url, scm="git", verbose=False):
        """ install a vendor in the couchapp dir.
        
        :attr url: string, url to retrieve vendor
        :attr scm: string, name of scm used to retrieve vendor. Default scm
        is git. You could add scm in ~/.couchapprc like this in `vendor_handlers`
        property :
            
        .. code-block:: javascript
            {
                "vendor_handlers": {
                    "scm": "/pathto/vendorscript"
                }
            }
            
        a vendor script receive 2 actions `install` and `update` and take differents arguments in stdin:
            * update: `vendorscript update url vendor_path vendor_dir`
            * install: `vendorscript install url vendor_dir`
        
        Errors should be returned to stderr. When installing the script should create a file named
        `.new` with url used to retrieve the vendor as first line.
        
        :attr verbose: boolean, False by default
        
        """
        if not scm in  self.vendor_handlers:
            print >>sys.stderr, "%s scm isn't supported yet." % scm
            sys.exit(-1)
              
        # get list of installed vendors
        installed = self.get_vendors()
                
        handler = self.vendor_handlers[scm]
        cmd = "%s install %s %s" % (handler, url, self.vendor_dir)
        (child_stdin, child_stdout, child_stderr) = _popen3(cmd)
        err = child_stderr.read()
        if verbose >=2:
            print child_stdout.read()
        if err:
            print >>sys.stderr, err
                
        # detect new vendor application and add url so we could update later
        for name in os.listdir(self.vendor_dir):
            current_path = os.path.join(self.vendor_dir, name)
            if os.path.isdir(current_path) and name not in installed:
                new_file = os.path.join(current_path, '.new')
                if os.path.isfile(new_file):
                    new_url = read_file(new_file).strip()
                    if new_url == url:
                        mfile = os.path.join(current_path, 'metadata.json')
                        write_json(mfile, {
                            "scm": scm,
                            "update_url": url
                        })
                        os.unlink(new_file)
                        return
                        
    def _update(self, name, verbose=False):
        current_path = os.path.join(self.vendor_dir, name)
        if os.path.isdir(current_path):
            mfile = os.path.join(current_path, 'metadata.json')
            metadata = read_json(mfile)
            if not metadata and name == 'couchapp':
                update_url = COUCHAPP_VENDOR_URL
                scm = COUCHAPP_VENDOR_SCM
            elif metadata:
                update_url = metadata['update_url']
                scm = metadata['scm']
                if not scm in VENDOR_HANDLERS:
                    scm = False
            
            if update_url and scm:
                # for now we manage only internal handlers
                handler = self.vendor_handlers[scm]
                if verbose >= 1:
                    print "Updating %s from %s" % (current_path, update_url)
                cmd = "%s update %s %s %s" % (handler, update_url, 
                                        current_path, self.vendor_dir)
                (child_stdin, child_stdout, child_stderr) = _popen3(cmd)
                if verbose >=2:
                    print child_stdout.read()
                    err = child_stderr.read()
                    if err:
                        print >>sys.stderr, err              
    
    def update(self, name=None, verbose=False): 
        """
        update vendor or all vendors if name is None
        
        :attr name: string, name of vendor
        :attr verbose: boolean, False by default
        """
        multiple = isinstance(name, (list, tuple,))
        for vendor_name in os.listdir(self.vendor_dir):
            if (multiple and vendor_name in name) or name is None:
                self._update(vendor_name, verbose=verbose)
            elif name and vendor_name == name:
                self._update(vendor_name, verbose=verbose)
                break