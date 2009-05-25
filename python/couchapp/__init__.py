#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# Copyright 2009 Benoit Chesneau <benoitc@e-engura.org>
#
# This software is licensed as described in the file LICENSE, which
# you should have received as part of this distribution.

try:
    __version__ = __import__('pkg_resources').get_distribution('Couchapp').version
except:
    __version__ = '?'

from couchapp.ui import UI
from couchapp.app import CouchApp
