import os
import tempfile
import sys
import unittest

from couchapp.ui import ui
from couchapp.utils import sign_file

from couchdb import Server, ResourceNotFound

class UITestCase(unittest.TestCase):
    
    def setUp(self):
        self.server = Server()
        self.db = self.server.create('couchapp-test')
        
        f, fname = tempfile.mkstemp()
        os.unlink(fname)
        self.tempdir = fname
        os.makedirs(self.tempdir)
        self.app_dir = os.path.join(self.tempdir, "test_couchapp")
        self.ui = ui(self.app_dir)
        
        
    def tearDown(self):
        del self.server['couchapp-test']
        for root, dirs, files in os.walk(self.tempdir, topdown=False):
            for name in files:
                os.remove(os.path.join(root, name))
            for name in dirs:
                os.rmdir(os.path.join(root, name))
                
    def testGenerate(self):
        self.ui.generate_app()
        
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
        self.ui.generate_app()
        self.ui.push_app("couchapp-test", "my-app")
        
        design_doc = None
        try:
            design_doc = self.db['_design/my-app']
        except ResourceNotFound:
            pass
        
        self.assert_(design_doc is not None)
        
        
        
        
        
        
if __name__ == '__main__':
    unittest.main()
        
        
        
    