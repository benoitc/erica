import unittest
import os

import couchapp.utils

class CouchAppTest(unittest.TestCase):

    def testInCouchApp(self):
        dir_, file_ = os.path.split(__file__)
        if dir_:
            os.chdir(dir_)

        startdir = os.getcwd()
        try:
            os.chdir("in_couchapp")
            os.chdir("installed")
            cwd = os.getcwd()
            self.assertEquals(couchapp.utils.in_couchapp(), cwd,
                              "in_couchapp() returns %s" % 
                              couchapp.utils.in_couchapp())
            os.chdir(os.path.pardir)
            os.chdir("no_install")
            self.assert_(not couchapp.utils.in_couchapp(),
                         "Found a couchapp at %s but didn't expect one!"
                         % couchapp.utils.in_couchapp())
        finally:
            os.chdir(startdir)

if __name__ == "__main__":
    unittest.main()
