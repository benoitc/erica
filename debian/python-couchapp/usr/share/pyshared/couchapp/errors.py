# -*- coding: utf-8 -*-
#
# This file is part of couchapp released under the Apache 2 license. 
# See the NOTICE for more information.

class AppError(Exception):
    """ raised when a application error appear """
    
class MacroError(Exception):
    """ raised for macro errors"""
    
class VendorError(Exception):
    """ vendor error """
    
class ResourceNotFound(Exception):
    """ raised when a resource not found on CouchDB"""
   
class ResourceConflict(Exception):
    """ raised when a conflict occured"""

class PreconditionFailed(Exception):
    """ precondition failed error """    
    
class RequestFailed(Exception): 
    """ raised when an http error occurs"""
    
class Unauthorized(Exception):
    """ raised when not authorized to access to CouchDB"""

class CommandLineError(Exception):
    """ error when a bad command line is passed"""
    
class BulkSaveError(Exception):
    """ error during bulk save"""
    
    def __init__(self, errors, *args, **kwargs):
        self.errors = errors
        super(BulkSaveError, self).__init__(*args, **kwargs)