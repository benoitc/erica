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

from couchapp.errors import ResourceNotFound
from couchapp.client import Database
from couchapp.util import popen3, deltree

couchapp_dir = os.path.join(os.path.dirname(__file__), '../')
couchapp_cli = os.path.join(os.path.dirname(__file__), '../bin/couchapp')
     
def _tempdir():
    f, fname = tempfile.mkstemp()
    os.unlink(fname)
    return fname
    
class CliTestCase(unittest.TestCase):
    
    def setUp(self):
        self.db = Database('http://127.0.0.1:5984/couchapp-test')
            
        self.tempdir = _tempdir()
        os.makedirs(self.tempdir)
        self.app_dir = os.path.join(self.tempdir, "my-app")
        self.cmd = "cd %s && couchapp" % self.tempdir
        self.startdir = os.getcwd()
        
    def tearDown(self):
        try:
            self.db.delete()
        except:
            pass
        deltree(self.tempdir)
        os.chdir(self.startdir)
        
    def _make_testapp(self):
        testapp_path = os.path.join(os.path.dirname(__file__), 'testapp')
        shutil.copytree(testapp_path, self.app_dir)
        
    def testGenerate(self):
        os.chdir(self.tempdir) 
        (child_stdin, child_stdout, child_stderr) = popen3("%s generate my-app" % self.cmd)
        appdir = os.path.join(self.tempdir, 'my-app')
        self.assert_(os.path.isdir(appdir) == True)
        cfile = os.path.join(appdir, '.couchapprc')
        self.assert_(os.path.isfile(cfile) == True)
        
        self.assert_(os.path.isdir(os.path.join(appdir, '_attachments')) == True)
        self.assert_(os.path.isfile(os.path.join(appdir, '_attachments', 'index.html')) == True)
        self.assert_(os.path.isfile(os.path.join(self.app_dir, '_attachments', 'style', 'main.css')))
        self.assert_(os.path.isdir(os.path.join(appdir, 'views')) == True)
        self.assert_(os.path.isdir(os.path.join(appdir, 'shows')) == True)
        self.assert_(os.path.isdir(os.path.join(appdir, 'lists')) == True)
        
    def testPush(self):
        self._make_testapp()
        (child_stdin, child_stdout, child_stderr) = popen3("%s push -v my-app couchapp-test" % self.cmd)
        
        # any design doc created ?
        design_doc = None
        try:
            design_doc = self.db.open_doc('_design/my-app')
        except ResourceNotFound:
            pass
        self.assert_(design_doc is not None)
        
        # should create view
        self.assert_('function' in design_doc['views']['example']['map'])
        
        # should use macros
        self.assert_('stddev' in design_doc['views']['example']['map'])
        self.assert_('ejohn.org' in design_doc['shows']['example-show'])
        self.assert_('included by foo.js' in design_doc['shows']['example-show'])
        
        # should create index
        self.assert_(design_doc['_attachments']['index.html']['content_type'] == 'text/html')
        
        # should create manifest
        self.assert_('foo/' in design_doc['couchapp']['manifest'])
        
        # should push and macro the doc shows
        self.assert_('Generated CouchApp Form Template' in design_doc['shows']['example-show'])
        
        # should push and macro the view lists
        self.assert_('Test XML Feed' in design_doc['lists']['feed'])
        
        # should allow deeper includes
        self.assertFalse('"helpers"' in design_doc['shows']['example-show'])
        
        # deep require macros
        self.assertFalse('"template"' in design_doc['shows']['example-show'])
        self.assert_('Resig' in design_doc['shows']['example-show'])
        
    def testPushNoAtomic(self):
        self._make_testapp()
        (child_stdin, child_stdout, child_stderr) = popen3("%s push --no-atomic my-app couchapp-test" % self.cmd)
        
        # any design doc created ?
        design_doc = None
        try:
            design_doc = self.db.open_doc('_design/my-app')
        except ResourceNotFound:
            pass
        self.assert_(design_doc is not None)
        
        # there are 3 revisions (1 doc creation + 2 attachments)
        self.assert_(design_doc['_rev'].startswith('3-'))
        
        # should create view
        self.assert_('function' in design_doc['views']['example']['map'])
        
        # should use macros
        self.assert_('stddev' in design_doc['views']['example']['map'])
        self.assert_('ejohn.org' in design_doc['shows']['example-show'])
        
        # should create index
        self.assert_(design_doc['_attachments']['index.html']['content_type'] == 'text/html')
        
        # should create manifest
        self.assert_('foo/' in design_doc['couchapp']['manifest'])
        
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
        (child_stdin, child_stdout, child_stderr) = popen3("%s push -v my-app couchapp-test" % self.cmd)
                
        design_doc = self.db.open_doc('_design/my-app')
        
        app_dir =  os.path.join(self.tempdir, "couchapp-test")
        
        (child_stdin, child_stdout, child_stderr) = popen3("%s clone %s %s" % (
                    self.cmd, "http://127.0.0.1:5984/couchapp-test/_design/my-app",
                    app_dir))
                    
        # should create .couchapprc
        self.assert_(os.path.isfile(os.path.join(app_dir, ".couchapprc")))
         
         
        # should clone the views
        self.assert_(os.path.isdir(os.path.join(app_dir, "views")))
        
        # should create foo/bar.txt file
        self.assert_(os.path.isfile(os.path.join(app_dir, 'foo/bar.txt')))
        
        # should create lib/helpers/math.js file
        self.assert_(os.path.isfile(os.path.join(app_dir, 'lib/helpers/math.js')))
        
        # should work when design doc is edited manually
        design_doc['test.txt'] = "essai"
        
        design_doc = self.db.save_doc(design_doc)
                
        deltree(app_dir)
        (child_stdin, child_stdout, child_stderr) = popen3("%s clone %s %s" % (self.cmd, 
                    "http://127.0.0.1:5984/couchapp-test/_design/my-app",
                    app_dir))
        self.assert_(os.path.isfile(os.path.join(app_dir, 'test.txt')))
        
        # should work when a view is added manually
        design_doc["views"]["more"] = { "map": "function(doc) { emit(null, doc); }" }
        
        design_doc = self.db.save_doc(design_doc)        
        
        deltree(app_dir)
        (child_stdin, child_stdout, child_stderr) = popen3("%s clone %s %s" % (
                    self.cmd, "http://127.0.0.1:5984/couchapp-test/_design/my-app",
                    app_dir))
        self.assert_(os.path.isfile(os.path.join(app_dir, 'views/example/map.js')))
        
        # should work without manifest
        del design_doc['couchapp']['manifest']
        design_doc = self.db.save_doc(design_doc)   
        deltree(app_dir)
        (child_stdin, child_stdout, child_stderr) = popen3("%s clone %s %s" % (
                    self.cmd, "http://127.0.0.1:5984/couchapp-test/_design/my-app",
                    app_dir))
        self.assert_(os.path.isfile(os.path.join(app_dir, 'views/example/map.js')))
        
        # should create foo/bar without manifest
        self.assert_(os.path.isfile(os.path.join(app_dir, 'foo/bar')))
        
        # should create lib/helpers.json without manifest
        self.assert_(os.path.isfile(os.path.join(app_dir, 'lib/helpers.json')))
            
    def testPushApps(self):
        os.chdir(self.tempdir)
        docsdir = os.path.join(self.tempdir, 'docs')
        os.makedirs(docsdir)

        # create 2 apps
        (child_stdin, child_stdout, child_stderr) = popen3(
                                            "%s generate docs/app1" % self.cmd)
        (child_stdin, child_stdout, child_stderr) = popen3(
                                            "%s generate docs/app2" % self.cmd)

        
        (child_stdin, child_stdout, child_stderr) = popen3(
            "%s pushapps docs/ http://127.0.0.1:5984/couchapp-test" % self.cmd)
        
        alldocs = self.db.all_docs()['rows']
        self.assert_(len(alldocs) == 2)
        self.assert_('_design/app1' == alldocs[0]['id'])
        
    def testPushDocs(self):
        os.chdir(self.tempdir)
        docsdir = os.path.join(self.tempdir, 'docs')
        os.makedirs(docsdir)
        
        # create 2 apps
        (child_stdin, child_stdout, child_stderr) = popen3("%s generate docs/app1" % self.cmd)
        (child_stdin, child_stdout, child_stderr) = popen3("%s generate docs/app2" % self.cmd)

        
        (child_stdin, child_stdout, child_stderr) = popen3("%s pushdocs docs/ http://127.0.0.1:5984/couchapp-test" % self.cmd)
       
        alldocs = self.db.all_docs()['rows']
        self.assert_(len(alldocs) == 2)
        
        self.assert_('_design/app1' == alldocs[0]['id'])
        
    def _make_testapp(self):
        testapp_path = os.path.join(os.path.dirname(__file__), 'testapp')
        shutil.copytree(testapp_path, self.app_dir)
        
if __name__ == '__main__':
    unittest.main()
