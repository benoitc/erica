#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# Copyright 2009 Benoit Chesneau <benoitc@e-engura.org>
#
# This software is licensed as described in the file LICENSE, which
# you should have received as part of this distribution.

import os
import tempfile
import shutil
import sys
import unittest

from couchapp.ui import UI
from couchapp.utils import popen3

from couchdb import Server, ResourceNotFound

def deltree(path):
    for root, dirs, files in os.walk(path, topdown=False):
        for name in files:
            os.remove(os.path.join(root, name))
        for name in dirs:
            os.rmdir(os.path.join(root, name))
    
couchapp_dir = os.path.join(os.path.dirname(__file__), '../python')
couchapp_cli = os.path.join(os.path.dirname(__file__), '../python/couchapp/bin/couchapp_cli.py')
     
            
class CliTestCase(unittest.TestCase):
    
    def setUp(self):
        self.server = Server()
        self.db = self.server.create('couchapp-test')
        
        f, fname = tempfile.mkstemp()
        os.unlink(fname)
        self.tempdir = fname
        os.makedirs(self.tempdir)
        self.app_dir = os.path.join(self.tempdir, "my-app")
        self.cmd = "cd %s && couchapp" % self.tempdir
        self.ui = UI(verbose=False)
        
    def tearDown(self):
        del self.server['couchapp-test']
        deltree(self.tempdir)
        
    def _make_testapp(self):
        testapp_path = os.path.join(os.path.dirname(__file__), 'testapp')
        shutil.copytree(testapp_path, self.app_dir)
                
    def testGenerate(self):
        (child_stdin, child_stdout, child_stderr) = popen3("%s generate my-app" % self.cmd)
        # should create application dir
        self.assert_(os.path.isdir(self.app_dir))
        # should create view dir
        self.assert_(os.path.isdir(os.path.join(self.app_dir, 'views')))
        # should create attachments dir
        self.assert_(os.path.isdir(os.path.join(self.app_dir, '_attachments')))
        # should create show dir
        self.assert_(os.path.isdir(os.path.join(self.app_dir, 'shows')))
        # should create vendor dir
        self.assert_(os.path.isdir(os.path.join(self.app_dir, 'vendor')))
        # deep attachment test
        self.assert_(os.path.isfile(os.path.join(self.app_dir, '_attachments', 'style/main.css')))
        
    def testPush(self):
        self._make_testapp()
        (child_stdin, child_stdout, child_stderr) = popen3("%s push my-app couchapp-test -v" % self.cmd)
        
        # any design doc created ?
        design_doc = None
        try:
            design_doc = self.db['_design/my-app']
        except ResourceNotFound:
            pass
        self.assert_(design_doc is not None)
        
        # should create view
        self.assert_('function' in design_doc['views']['example']['map'])
        
        # should use macros
        self.assert_('stddev' in design_doc['views']['example']['map'])
        self.assert_('ejohn.org' in design_doc['shows']['example-show'])
        
        # should create index
        self.assert_(design_doc['_attachments']['index.html']['content_type'] == 'text/html')
        
        # should create manifest
        self.assert_('foo' in design_doc['couchapp']['manifest'][0])
        
        # should push and macro the doc shows
        self.assert_('Generated CouchApp Form Template' in design_doc['shows']['example-show'])
        
        # should push and macro the view lists
        self.assert_('Test XML Feed' in design_doc['lists']['feed'])
        
        # should allow deeper includes
        self.assertFalse('"helpers"' in design_doc['shows']['example-show'])
        
        # deep require macros
        self.assertFalse('"template"' in design_doc['shows']['example-show'])
        self.assert_('Resig' in design_doc['shows']['example-show'])
        
    def testClone(self):
        self._make_testapp()
        (child_stdin, child_stdout, child_stderr) = popen3("%s push my-app couchapp-test" % self.cmd)
        
        design_doc = self.db['_design/my-app']
        
        app_dir =  os.path.join(self.tempdir, "test_couchapp2")
        
        (child_stdin, child_stdout, child_stderr) = popen3("%s clone %s %s" % (
                    self.cmd, "http://127.0.0.1:5984/couchapp-test/_design/my-app",
                    app_dir))
        # should clone the views
        self.assert_(os.path.isdir(os.path.join(app_dir, "views")))
        
        # should create foo/bar.txt file
        self.assert_(os.path.isfile(os.path.join(app_dir, 'foo/bar.txt')))
        
        # should create lib/helpers/math.js file
        self.assert_(os.path.isfile(os.path.join(app_dir, 'lib/helpers/math.js')))
        
        # should work when design doc is edited manually
        design_doc['test.txt'] = "essai"
        self.db['_design/my-app'] = design_doc
        deltree(app_dir)
        (child_stdin, child_stdout, child_stderr) = popen3("%s clone %s %s" % (self.cmd, 
                    "http://127.0.0.1:5984/couchapp-test/_design/my-app",
                    app_dir))
        self.assert_(os.path.isfile(os.path.join(app_dir, 'test.txt')))
        
        # should work when a view is added manually
        design_doc["views"]["more"] = { "map": "function(doc) { emit(null, doc); }" }
        self.db['_design/my-app'] = design_doc
        deltree(app_dir)
        (child_stdin, child_stdout, child_stderr) = popen3("%s clone %s %s" % (
                    self.cmd, "http://127.0.0.1:5984/couchapp-test/_design/my-app",
                    app_dir))
        self.assert_(os.path.isfile(os.path.join(app_dir, 'views/example/map.js')))
        
        # should work without manifest
        del design_doc['couchapp']['manifest']
        self.db['_design/my-app'] = design_doc
        deltree(app_dir)
        (child_stdin, child_stdout, child_stderr) = popen3("%s clone %s %s" % (
                    self.cmd, "http://127.0.0.1:5984/couchapp-test/_design/my-app",
                    app_dir))
        self.assert_(os.path.isfile(os.path.join(app_dir, 'views/example/map.js')))
        
        # should create foo/bar without manifest
        self.assert_(os.path.isfile(os.path.join(app_dir, 'foo/bar')))
        
        # should create lib/helpers.json without manifest
        self.assert_(os.path.isfile(os.path.join(app_dir, 'lib/helpers.json')))
        
if __name__ == '__main__':
    unittest.main()