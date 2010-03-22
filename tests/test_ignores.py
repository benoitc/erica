# -*- coding: utf-8 -*-
#
# Copyright 2008,2009 Benoit Chesneau <benoitc@e-engura.org>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at#
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

import unittest
import tempfile
import json
import os
from shutil import rmtree
from couchapp.localdoc import LocalDoc as doc

class IgnoresTests(unittest.TestCase):

    def setUp(self):
        # Create a temp dir for the tests to run in
        self.tmp_dir = tempfile.mkdtemp()
        # Define some test data
        self.testdata = {'CVS': True, "dontignorethisCVS": False, 
                        "ignore_me": True, "but_don't_ignore_me": False}
        # Create the ignores file
        self.ignores = ["^CVS", "ignore_me"]
        f = open(os.path.join(self.tmp_dir, '.couchappignore'), 'w')
        json.dump(self.ignores, f)
        f.close()

        # Make a UI and a doc instance for the tests
        self.doc = doc(self.tmp_dir)

        # I could write these files to the temp area, but that seems unnecessary
        # since the unit test doesn't interact with the file system other than 
        # to make the .couchappignore file.
        #for i in self.testdata.keys():
        #   open(os.path.join(self.tmp_dir, i), 'w').close()
        
    def tearDown(self):
        # Clear up temp dir and the files it contains
        rmtree(self.tmp_dir)
    
    def testLoadIgnores(self):
        """
        If the code works the doc should have a data member containing a list of 
        the regexps to ignore, and this list should be the same as the list stored
        in self.ignores and used in setUp to create the .couchappignore file.
        """
        assert self.doc.ignores == self.ignores
    
    def testIgnore(self):
        """
        Run through the test data and check that the doc would treat it appropriately
        were it a file/directory the doc was uploading.
        
        Really this test checks the re module...
        """
        for i in self.testdata.keys():
            assert self.doc.check_ignore(i) == self.testdata[i]
        
if __name__ == '__main__':
    unittest.main()