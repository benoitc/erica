# -*- coding: utf-8 -*-
#
# Copyright 2008,2009 Benoit Chesneau <benoitc@e-engura.org>
#
#  Licensed under the Apache License, Version 2.0 (the "License");
#  you may not use this file except in compliance with the License.
#  You may obtain a copy of the License at#
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
#  Unless required by applicable law or agreed to in writing, software
#  distributed under the License is distributed on an "AS IS" BASIS,
#  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#  See the License for the specific language governing permissions and
#  limitations under the License.

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