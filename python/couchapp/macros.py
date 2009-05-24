#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# Copyright 2009 Benoit Chesneau <benoitc@e-engura.org>
#
# This software is licensed as described in the file LICENSE, which
# you should have received as part of this distribution.
#
import glob
from hashlib import md5
import os
import re
import sys
# python 2.6
try:
    import json 
except ImportError:
    import simplejson as json

from couchapp.utils import read_file, read_json, to_bytestring

def package_shows(doc, funcs, app_dir, objs, verbose=False):
   apply_lib(doc, funcs, app_dir, objs, verbose=verbose)
         
def package_views(doc, views, app_dir, objs, verbose=False):
   for view, funcs in views.iteritems():
       apply_lib(doc, funcs, app_dir, objs, verbose=verbose)

def apply_lib(doc, funcs, app_dir, objs, verbose=False):
    for k, v in funcs.iteritems():
        if not isinstance(v, basestring):
            continue
        old_v = v
        try:
            funcs[k] = run_json_macros(doc, 
                run_code_macros(v, app_dir, verbose=verbose), 
                app_dir, verbose=verbose)
        except ValueError, e:
            print >>sys.stderr, "Error running !code or !json on function \"%s\": %s" % (k, e)
            sys.exit(-1)
        if old_v != funcs[k]:
            objs[md5(to_bytestring(funcs[k])).hexdigest()] = old_v
           

def run_code_macros(f_string, app_dir, verbose=False):
   def rreq(mo):
       # just read the file and return it
       path = os.path.join(app_dir, mo.group(2).strip(' '))
       library = ''
       filenum = 0
       for filename in glob.iglob(path):            
           if verbose>=2:
               print "process code macro: %s" % filename
           try:
               library += read_file(filename)
           except IOError, e:
               print >>sys.stderr, e
               sys.exit(-1)
           filenum += 1
           
       if not filenum:
           print >>sys.stderr, "Processing code: No file matching '%s'" % mo.group(2)
           sys.exit(-1)
           
       return library

   re_code = re.compile('(\/\/|#)\ ?!code (.*)')
   return re_code.sub(rreq, f_string)

def run_json_macros(doc, f_string, app_dir, verbose=False):
   included = {}
   varstrings = []

   def rjson(mo):
       if mo.group(2).startswith('_attachments'): 
           # someone  want to include from attachments
           path = os.path.join(app_dir, mo.group(2).strip(' '))
           filenum = 0
           for filename in glob.iglob(path):
               library = ''
               try:
                   if filename.endswith('.json'):
                       library = read_json(filename)
                   else:
                       library = read_file(filename)
               except IOError, e:
                   print >>sys.stderr, e
                   sys.exit(1)
               filenum += 1
               current_file = filename.split(app_dir)[1]
               fields = current_file.split('/')
               count = len(fields)
               include_to = included
               for i, field in enumerate(fields):
                   if i+1 < count:
                       include_to[field] = {}
                       include_to = include_to[field]
                   else:
                       include_to[field] = library
           if not filenum:
               print >>sys.stderr, "Processing code: No file matching '%s'" % mo.group(2)
               sys.exit(-1)
       else:	
           fields = mo.group(2).split('.')
           library = doc
           count = len(fields)
           include_to = included
           for i, field in enumerate(fields):
               if not field in library: break
               library = library[field]
               if i+1 < count:
                   include_to[field] = include_to.get(field, {})
                   include_to = include_to[field]
               else:
                   include_to[field] = library

       return f_string

   def rjson2(mo):
       return '\n'.join(varstrings)

   re_json = re.compile('(\/\/|#)\ ?!json (.*)')
   re_json.sub(rjson, f_string)

   if not included:
       return f_string

   for k, v in included.iteritems():
       varstrings.append("var %s = %s;" % (k, json.dumps(v)))

   return re_json.sub(rjson2, f_string)