#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# Copyright 2009 Benoit Chesneau <benoitc@e-engura.org>
#
# This software is licensed as described in the file COPYING, which
# you should have received as part of this distribution.

import os
import sys

from setuptools import setup


data_files = []
root_dir = os.path.dirname(__file__)
if root_dir != '':
    os.chdir(root_dir)

for dirpath, dirnames, filenames in os.walk('app-template'):
    for i, dirname in enumerate(dirnames):
        if dirname.startswith('.'): del dirnames[i]
    
    data_files.append([dirpath, [os.path.join(dirpath, f) for f in filenames]])



setup(
    name = 'Couchapp',
    version = '0.1.4',
    url = 'http://github.com/benoitc/couchapp/tree/master',
    license =  'Apache License 2',
    author = 'Benoit Chesneau',
    author_email = 'benoitc@e-engura.org',
    description = 'Standalone CouchDB Application Development Made Simple.',
    long_description = """CouchApp is a set of helpers and a jQuery plugin
    that conspire to get you up and running on CouchDB quickly and
    correctly. It brings clarity and order to the freedom of CouchDB’s
    document-based approach.""",
    keywords = 'couchdb couchapp',
    platforms = 'any',
    zip_safe = False,
    
    packages= ['couchapp'],
    package_dir={'couchapp': 'python/couchapp'},
    data_files = data_files,
    include_package_data = True,
    scripts = ['python/couchapp/bin/couchapp'],
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
        'setuptools>=0.6c9',
        'couchdb>=0.5',
    ]
)

