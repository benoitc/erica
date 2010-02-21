# -*- coding: utf-8 -*-
#
# This file is part of couchapp released under the Apache 2 license. 
# See the NOTICE for more information.

import os
import shutil
import sys

from couchapp.errors import AppError
from couchapp import localdoc
from couchapp.utils import user_path, relpath

__all__ = ["generate_app", "generate_function"]

def generate_app(ui, path, template=None, create=False):
    """ Generates a CouchApp in app_dir 
    
    :attr verbose: boolean, default False
    :return: boolean, dict. { 'ok': True } if ok, { 'ok': False, 'error': message } 
    if something was wrong.
    """
    DEFAULT_APP_TREE = [
        '_attachments',
        'lists',
        'shows',
        'updates',
        'views'
    ]
    
    TEMPLATES = ['app']
    prefix = ''
    if template is not None:
        prefix = os.path.join(*template.split('/'))
    try:
        os.makedirs(path)
    except OSError, e:
        errno, message = e
        raise AppError("Can't create a CouchApp in %s: %s" % (
                path, message))
    
    for n in DEFAULT_APP_TREE:
        tp = os.path.join(path, n)
        os.makedirs(tp)
    
    for t in TEMPLATES:
        appdir = path
        if prefix:
            # we do the job twice for now to make sure an app or vendor
            # template exist in user template location
            # fast on linux since there is only one user dir location
            # but could be a little slower on windows
            for user_location in user_path():
                location = os.path.join(user_location, 'templates', prefix, t)
                if os.path.exists(location):
                    t = os.path.join(prefix, t)
                    break
            
        copy_helper(appdir, t)
        
    # add vendor
    vendor_dir = os.path.join(appdir, 'vendor', 'couchapp')
    os.makedirs(vendor_dir)
    copy_helper(vendor_dir, '', tname="vendor")
    
    fid = os.path.join(appdir, '_id')
    if not os.path.isfile(fid):
        f = open(fid, 'wb')
        f.write('_design/%s' % os.path.split(appdir)[1])
        f.close()
    
    if create:
        doc = localdoc.instance(ui, path, create=True)

    #ui.extensions.notify("post-generate", path)
    
def generate_function(ui, path, kind, name, template=None):
    functions_path = ['functions']
    if template:
        functions_path = []
        _relpath = os.path.join(*template.split('/'))
        template_dir = find_template_dir("templates", _relpath)
    else:
        template_dir = find_template_dir("templates")
    if template_dir:
        functions = []
        if kind == "view":
            path = os.path.join(path, "%ss" % kind, name)
            if os.path.exists(path):
                raise AppError("The view %s already exists" % name)
            functions = [('map.js', 'map.js'), ('reduce.js', 'reduce.js')]
        elif kind == "function":
            functions = [('%s.js' % name, '%s.js' % name)]
        elif kind == "vendor":
            app_dir = os.path.join(path, "vendor", name)
            try:
                os.makedirs(app_dir)
            except:
                pass
            targetpath = os.path.join(*template.split('/'))
            copy_helper(path, targetpath)
            return
        else:
            path = os.path.join(path, "%ss" % kind)
            functions = [('%s.js' % kind, "%s.js" % name )]
        try:
            os.makedirs(path)
        except:
            pass
        
        for template, target in functions:
            target_path = os.path.join(path, target)
            root_path = [template_dir] + functions_path + [template]
            root = os.path.join(*root_path)
            try:
                shutil.copy2(root, target_path)
            except:
                ui.logger.info("%s not found in %s" % (template, os.path.join(*root_path[:-1])))
    else:
        raise AppError("Defaults templates not found. Check your install.")
        

def copy_helper(path, directory, tname="templates"):
    """ copy helper used to generate an app"""
    templatedir = find_template_dir(tname, directory)
    if templatedir:
        if directory == "vendor":
            path = os.path.join(path, directory)
            try:
                os.makedirs(path)
            except:
                pass
        
        for root, dirs, files in os.walk(templatedir):
            rel = relpath(root, templatedir)
            if rel == ".":
                rel = ""
            target_path = os.path.join(path, rel)
            for d in dirs:
                try:
                    os.makedirs(os.path.join(target_path, d))
                except:
                    continue
            for f in files:
                shutil.copy2(os.path.join(root, f), os.path.join(target_path, f))                
    else:
        raise AppError("Can't create a CouchApp in %s: default template not found." % (
                path))
                        
def find_template_dir(name, directory=''):
    paths = ['%s' % name, '../%s' % name]
    if hasattr(sys, 'frozen'): # py2exe
        modpath = sys.executable
    else:
        modpath = __file__
        
    default_locations = [os.path.join(os.path.dirname(modpath), p, directory) for p in paths]
    
    if directory:
        user_locations = []
        for user_location in user_path():
            user_locations.append(os.path.join(user_location, name, directory))
        default_locations = user_locations + default_locations

    found = False
    for location in default_locations:
        template_dir = os.path.normpath(location)
        if os.path.isdir(template_dir):
            found = True
            break
    if found:
        return template_dir
    return False