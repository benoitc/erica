#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# Copyright 2009 Benoit Chesneau <benoitc@e-engura.org>
#
# This software is licensed as described in the file LICENSE, which
# you should have received as part of this distribution.
#

class AppError(Exception):
    """ raised when a application error appear """
    
class MacroError(Exception):
    """ raised for macro errors"""
    
class VendorError(Exception):
    """ vendor error """