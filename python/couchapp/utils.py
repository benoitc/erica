#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# Copyright 2009 Benoit Chesneau <benoitc@e-engura.org>
#
# This software is licensed as described in the file LICENSE, which
# you should have received as part of this distribution.
#

import os

def in_couchapp():
    current_path = os.getcwd()
    old_dirs = []
    while 1:
        dirs = os.listdir(current_path)
        if dirs == old_dirs: 
            return False
        if '.couchapp' in dirs: break
        current_path = os.path.normpath(os.path.join(current_path, '../'))
        old_dirs = dirs
    return current_path

