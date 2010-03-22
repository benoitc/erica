# -*- coding: utf-8 -*-
#
# This file is part of couchapp released under the Apache 2 license. 
# See the NOTICE for more information.


from couchapp.restkit import ResourceError

class AppError(Exception):
    """ raised when a application error appear """
    
class MacroError(Exception):
    """ raised for macro errors"""
    
class VendorError(Exception):
    """ vendor error """
    
class ResourceNotFound(ResourceError):
    """ raised when a resource not found on CouchDB"""
   
class ResourceConflict(ResourceError):
    """ raised when a conflict occured"""

class PreconditionFailed(ResourceError):
    """ precondition failed error """    
    
class RequestFailed(Exception): 
    """ raised when an http error occurs"""
    
class Unauthorized(Exception):
    """ raised when not authorized to access to CouchDB"""

class CommandLineError(Exception):
    """ error when a bad command line is passed"""
    
class BulkSaveError(Exception):
    """ error raised when therer are conflicts in bulk save"""
    
    def ___init__(self, docs, errors):
        Exception.__init__(self)
        self.docs = docs
        self.errors = errors
        
class ScriptError(Exception):
    """ exception raised in external script"""
    
class InvalidAttachment(Exception):
    """ raised when attachment is invalid (bad size, ct, ..)"""