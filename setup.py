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

import os
import sys

data_files = []

for dir, dirs, files in os.walk('app-template'):
    data_files.append((os.path.join('couchapp', dir), 
        [os.path.join(dir, file_) for file_ in files]))

for dir, dirs, files in os.walk('python/couchapp'):
    for i, dirname in enumerate(dirs):
        if dirname.startswith('.'): del dirs[i]
    
    data_files.append((os.path.join('couchapp', dir), 
        [os.path.join(dir, file_) for file_ in files]))

setup(
    name = 'Couchapp',
    version = '0.1.10',
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
    platforms = 'any',
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

    install_requires = ['couchdb'],
)

