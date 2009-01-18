#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# Copyright 2009 Benoit Chesneau <benoitc@e-engura.org>
#
# This software is licensed as described in the file COPYING, which
# you should have received as part of this distribution.

import os
import sys
import shutil

from setuptools import setup

def fullsplit(path, result=None):
    """
    Split a pathname into components (the opposite of os.path.join) in a
    platform-neutral way.
    """
    if result is None:
        result = []
    head, tail = os.path.split(path)
    if head == '':
        return [tail] + result
    if head == path:
        return result
    return fullsplit(head, [tail] + result)

packages, data_files = [], []
root_dir = os.path.join(os.path.dirname(__file__), 'python')
if root_dir != '':
    os.chdir(root_dir)




couchapp_dir = 'couchapp'

for dirpath, dirnames, filenames in os.walk(couchapp_dir):
    for i, dirname in enumerate(dirnames):
        if dirname.startswith('.'): del dirnames[i]
    if '__init__.py' in filenames:
        packages.append('.'.join(fullsplit(dirpath)))
    elif filenames:
        data_files.append([dirpath, [os.path.join(dirpath, f) for f in filenames]])

# add templates to dir
templates_path = os.path.abspath(os.path.join(os.getcwd(),
    '../app-template'))


for dirpath, dirnames, filenames in os.walk('../app-template'):
    for i, dirname in enumerate(dirnames):
        if dirname.startswith('.'): del dirnames[i]
    
    name = dirpath[3:]
    data_files.append([name, [os.path.join(dirpath, f) for f in filenames]])



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
    scripts = ['couchapp/bin/couchapp'],
    packages= packages,
    data_files = data_files,
    include_package_data = True,
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
        'setuptools',
        'couchdb',
    ]
)

