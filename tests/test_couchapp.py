#!/usr/bin/env python
# -*- coding: utf-8 -*-
#Copyright 2009 by Benoît Chesneau <benoitc@e-engura.com>
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

import os
import tempfile
import sys
import unittest

from couchapp.app import Couchapp
from couchapp.utils import sign_file

class CouchappTestCase(unittest.TestCase):
    
    def setUp(self):
        f, fname = tempfile.mkstemp()
        os.unlink(fname)
        self.tempdir = fname
        os.makedirs(self.tempdir)
        self.app_dir = os.path.join(self.tempdir, "test_couchapp")
        self.app = Couchapp(self.app_dir)
        
    def tearDown(self):
        for root, dirs, files in os.walk(self.tempdir, topdown=False):
            for name in files:
                os.remove(os.path.join(root, name))
            for name in dirs:
                os.rmdir(os.path.join(root, name))
                
    def testInitialize(self):
        os.makedirs(self.app_dir)
        ret = self.app.initialize()
        self.assert_(ret['ok'] == True)
        self.assert_(os.path.isfile(os.path.join(self.app_dir, '.couchapprc')))
        
    def testGenerate(self):
        ret = self.app.generate()
        self.assert_(ret['ok'] == True)
        self.assert_(os.path.isdir(os.path.join(self.app_dir, '_attachments')))
        self.assert_(os.path.isfile(os.path.join(self.app_dir, 'shows/example-show.js')))
        
    def testPush(self):
        ret = self.app.generate()
        self.assert_(ret['ok'] == True)
        
        design_doc = self.app.to_designdoc("couchapp_test")
        self.assert_(isinstance(design_doc, dict))
        self.assert_('_id' in design_doc)
        self.assert_(design_doc['_id'] == "_design/couchapp_test")
        self.assert_('lib' in design_doc)
        self.assert_('helpers' in design_doc['lib'])
        self.assert_('template' in design_doc['lib']['helpers'])
        
    def testPushView(self):
        ret = self.app.generate()
        design_doc = self.app.to_designdoc("couchapp_test")
        self.assert_('views' in design_doc)
        self.assert_('example' in design_doc['views'])
        self.assert_('map' in design_doc['views']['example'])
        self.assert_('emit' in design_doc['views']['example']['map'])
        
    def testPushCouchApp(self):
        ret = self.app.generate()
        design_doc = self.app.to_designdoc("couchapp_test")
        self.assert_('couchapp' in design_doc)
        
    def testPushManifest(self):
        ret = self.app.generate()
        design_doc = self.app.to_designdoc("couchapp_test")
        self.assert_('manifest' in design_doc['couchapp'])
        self.assert_('lib/helpers/template.js' in design_doc['couchapp']['manifest'])
        self.assert_('foo/' in design_doc['couchapp']['manifest'])
        self.assert_(len(design_doc['couchapp']['manifest']) == 23)
        
        
    def testAttachments(self):
        ret = self.app.generate()
        design_doc = self.app.to_designdoc("couchapp_test")
        self.assert_('_attachments' in design_doc)
        self.assert_('index.html' in design_doc['_attachments'])
        self.assert_('style/main.css' in design_doc['_attachments'])
        
        self.assert_(hasattr(design_doc['_attachments']['style/main.css'], 'read'))
        content = design_doc['_attachments']['style/main.css'].read()
        self.assert_(content == "/* add styles here */")
        
    def testSignatures(self):
        ret = self.app.generate()
        design_doc = self.app.to_designdoc("couchapp_test")
        self.assert_('signatures' in design_doc['couchapp'])
        self.assert_(len(design_doc['couchapp']['signatures']) == 2)
        self.assert_('index.html' in design_doc['couchapp']['signatures'])
        signature =  design_doc['couchapp']['signatures']['index.html']
        fsignature = sign_file(os.path.join(self.app_dir, '_attachments/index.html'))
        self.assert_(signature==fsignature)
        
    def testClone(self):
        ret = self.app.generate()
        design_doc = self.app.to_designdoc("couchapp_test")
        
        app_dir = os.path.join(self.tempdir, "test_couchapp2")
        app = Couchapp(app_dir)
        
        ret = app.clone(design_doc)
        self.assert_(ret['ok'] == True)
        
        self.assert_(os.path.isfile(os.path.join(app_dir, '_attachments/index.html')))
        self.assert_(os.path.isfile(os.path.join(app_dir, 'views/example/map.js')))

    def testClone2(self):
        ret = self.app.generate()
        design_doc = self.app.to_designdoc("couchapp_test")

        os.chdir(self.tempdir)
        app_dir = ''
        app = Couchapp(app_dir)

        ret = app.clone(design_doc)
        self.assert_(ret['ok'] == True)
        
        self.assert_(os.path.isfile('couchapp_test/_attachments/index.html'))
        self.assert_(os.path.isfile('couchapp_test/views/example/map.js'))
        
if __name__ == '__main__':
    unittest.main()
