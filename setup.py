#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# Copyright 2009 Benoit Chesneau <benoitc@e-engura.org>
#
# This software is licensed as described in the file COPYING, which
# you should have received as part of this distribution.

import os
import sys

from ez_setup import use_setuptools
if 'cygwin' in sys.platform.lower():
   min_version='0.6c6'
else:
   min_version='0.6a9'
try:
    use_setuptools(min_version=min_version)
except TypeError:
    # If a non-local ez_setup is already imported, it won't be able to
    # use the min_version kwarg and will bail with TypeError
    use_setuptools()

from setuptools import setup, find_packages, Extension, Feature
from setuptools.command.easy_install import easy_install
from distutils.command.build_ext import build_ext
from distutils.errors import CCompilerError, DistutilsExecError, \
    DistutilsPlatformError

data_files = []

for dir, dirs, files in os.walk('app-template'):
    data_files.append((os.path.join('couchapp', dir), 
        [os.path.join(dir, file_) for file_ in files]))

for dir, dirs, files in os.walk('vendor'):
    data_files.append((os.path.join('couchapp', dir), 
        [os.path.join(dir, file_) for file_ in files]))

for dir, dirs, files in os.walk('src/couchapp'):
    for i, dirname in enumerate(dirs):
        if dirname.startswith('.'): del dirs[i]
        
    data_files.append((dir, [os.path.join(dir, file_) for file_ in files]))
    
easy_install.real_process_distribution = easy_install.process_distribution
def process_distribution(self, *args, **kwargs):
    """ overide process_distribution to add permissions"""
    easy_install.real_process_distribution(self, *args, **kwargs)
    import pkg_resources
    external_path = '/pathto/couchapp/_external'
    try:
        pkg_resources.require('couchapp')
        external_path = pkg_resources.resource_filename("couchapp", "_external")
        for dir, dirs, files in os.walk(external_path):
            for i, dirname in enumerate(dirs):
                if dirname.startswith('.'): del dirs[i]
            for file_ in files:
                os.chmod(os.path.join(dir, file_), 0755)  
            
    except:
        print >>sys.stderr, "Chmoding failed. Try to 'chmod -R +x %s'" % external_path
easy_install.process_distribution = process_distribution

speedups = Feature(
    "options C speed-enhancement modules",
    standard=True,
    ext_modules = [
        Extension("couchapp/contrib/simplejson._speedups", ["src/couchapp/contrib/simplejson/_speedups.c"]),
    ],
)

if sys.platform == 'win32' and sys.version_info > (2, 6):
   # 2.6's distutils.msvc9compiler can raise an IOError when failing to
   # find the compiler
   ext_errors = (CCompilerError, DistutilsExecError, DistutilsPlatformError,
                 IOError)
else:
   ext_errors = (CCompilerError, DistutilsExecError, DistutilsPlatformError)

class BuildFailed(Exception):
    pass

class ve_build_ext(build_ext):
    # This class allows C extension building to fail.

    def run(self):
        try:
            build_ext.run(self)
        except DistutilsPlatformError, x:
            raise BuildFailed()

    def build_extension(self, ext):
        try:
            build_ext.build_extension(self, ext)
        except ext_errors, x:
            raise BuildFailed() 
 
def run_setup(with_binary):
    if with_binary:
        features = {'speedups': speedups}
    else:
        features = {}
    
    setup(
        name = 'Couchapp',
        version = '0.3',
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

        zip_safe = False,
    
        packages=find_packages('src'),
        package_dir={
            '': 'src'
        },
        data_files = data_files,
        include_package_data = True,
        entry_points = {
            'console_scripts': [
                'couchapp = couchapp.bin.couchapp_cli:main',
            ]
        },
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
        features=features,
        cmdclass={'build_ext': ve_build_ext},
        test_suite='tests',
    
    )


try:
    run_setup(True)
except BuildFailed:
    BUILD_EXT_WARNING = "WARNING: The C extension could not be compiled, speedups are not enabled."
    print '*' * 75
    print BUILD_EXT_WARNING
    print "Failure information, if any, is above."
    print "I'm retrying the build without the C extension now."
    print '*' * 75

    run_setup(False)

    print '*' * 75
    print BUILD_EXT_WARNING
    print "Plain-Python installation succeeded."
    print '*' * 75
