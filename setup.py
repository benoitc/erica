# -*- coding: utf-8 -*-
#
# This file is part of couchapp released under the Apache 2 license. 
# See the NOTICE for more information.

from distutils.command.install_data import install_data
import os
import sys

if not hasattr(sys, 'version_info') or sys.version_info < (2, 5, 0, 'final'):
    raise SystemExit("Couchapp requires Python 2.5 or later.")

from setuptools import setup, find_packages

    
extra = {}
data_files = []
for root in ('templates', 'vendor'):
    for dir, dirs, files in os.walk(root):
        dirs[:] = [x for x in dirs if not x.startswith('.')]
        files = [x for x in files if not x.startswith('.')]
        data_files.append((os.path.join('couchapp', dir),
                          [os.path.join(dir, file_) for file_ in files]))

class install_package_data(install_data):
    def finalize_options(self):
        self.set_undefined_options('install',
                                   ('install_lib', 'install_dir'))
        install_data.finalize_options(self)
 
cmdclass = {'install_data': install_package_data }

if os.name == "nt":
    # py2exe needs to be installed to work
    try:
        import py2exe

        # Help py2exe to find win32com.shell
        try:
            import modulefinder
            import win32com
            for p in win32com.__path__[1:]: # Take the path to win32comext
                modulefinder.AddPackagePath("win32com", p)
            pn = "win32com.shell"
            __import__(pn)
            m = sys.modules[pn]
            for p in m.__path__[1:]:
                modulefinder.AddPackagePath(pn, p)
        except ImportError:
            raise SystemExit('You need pywin32 installed ' +
                    'http://sourceforge.net/projects/pywin32')
        extra['console'] = ['bin/couchapp.py']
    except ImportError:
        raise SystemExit('You need py2exe installed to run Couchapp.')
    

from couchapp import __version__
 
setup(
    name = 'Couchapp',
    version = __version__,
    url = 'http://github.com/couchapp/couchapp/tree/master',
    license =  'Apache License 2',
    author = 'Benoit Chesneau',
    author_email = 'benoitc@e-engura.org',
    description = 'Standalone CouchDB Application Development Made Simple.',
    long_description = """CouchApp is a set of helpers and a jQuery plugin
    that conspire to get you up and running on CouchDB quickly and
    correctly. It brings clarity and order to the freedom of CouchDB's
    document-based approach.""",
    keywords = 'couchdb couchapp',
    platforms = ['any'],
    classifiers = [
        'License :: OSI Approved :: Apache Software License',
        'Intended Audience :: Developers',
        'Intended Audience :: System Administrators',
        'Development Status :: 4 - Beta',
        'Programming Language :: Python',
        'Operating System :: OS Independent',
        'Topic :: Database',
        'Topic :: Utilities',
    ],

    packages= find_packages(exclude=['tests']),
    data_files=data_files,

    include_package_data = True,
    cmdclass=cmdclass,
    
    install_requires = [
        'setuptools>=0.6b1'
    ],
    
    options = dict(py2exe={},
                   bdist_mpkg=dict(zipdist=True,
                                   license='LICENSE',
                                   readme='contrib/macosx/Readme.html',
                                   welcome='contrib/macosx/Welcome.html')),
                                   
    entry_points="""
    [couchapp.vendor]
    git=couchapp.vendors.backends.git:GitVendor
    hg=couchapp.vendors.backends.hg:HgVendor
    couchdb=couchapp.vendors.backends.couchdb:CouchdbVendor
    
    [couchapp.hook]
    compress=couchapp.hooks.compress:hook
    
    [console_scripts]
    couchapp=couchapp.dispatch:run
    """,
    
    test_suite='tests',
    **extra
)
