#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# Copyright 2009 Benoit Chesneau <benoitc@e-engura.org>
#
# This software is licensed as described in the file COPYING, which
# you should have received as part of this distribution.
import ez_setup
ez_setup.use_setuptools()

from setuptools import setup, find_packages
from setuptools.command.easy_install import easy_install

import os
import sys

data_files = []

for dir, dirs, files in os.walk('app-template'):
    data_files.append((os.path.join('couchapp', dir), 
        [os.path.join(dir, file_) for file_ in files]))

for dir, dirs, files in os.walk('vendor'):
    data_files.append((os.path.join('couchapp', dir), 
        [os.path.join(dir, file_) for file_ in files]))

for dir, dirs, files in os.walk('python/couchapp'):
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
 
setup(
    name = 'Couchapp',
    version = '0.2',
    url = 'http://github.com/benoitc/couchapp/tree/master',
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
    
    packages=find_packages('python'),
    package_dir={
        '': 'python'
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

    setup_requires = [
        'setuptools>=0.6c7',
    ],

    install_requires = [
        'couchdb>=0.5',
        'simplejson',
    ],
    
)

