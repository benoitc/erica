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

import os

import couchapp
from couchapp.utils import user_path


def generate_app(ui, path, template=None):
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
    
    TEMPLATES = ['app', 'vendor']
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
        tp = self.ui.rjoin(path, n)
        os.makedirs(tp)
    
    for t in TEMPLATES:
        app_dir = self.app_dir
        if prefix:
            # we do the job twice for now to make sure an app or vendor
            # template exist in user template location
            # fast on linux since there is only one user dir location
            # but could be a little slower on windows
            for user_location in user_path():
                location = os.path.join(user_location, 'templates', prefix, t)
                if os.path.exists(location):
                    if t == "vendor":
                        app_dir = os.path.join(path, "vendor")
                        try:
                            os.makedirs(path)
                        except:
                            pass
                    t = os.path.join(prefix, t)
                    break
            
        ui.copy_helper(app_dir, t)

    ui.extensions.notify("post-generate", path)
    
def generate_function(ui, path, kind, name, template=None):
    functions_path = ['functions']
    if template is not None:
        functions_path = []
        relpath = os.path.join(*template.split('/'))
        template_dir = ui.find_template_dir(relpath)
    else:
        template_dir = ui.find_template_dir()
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
            ui.copy_helper(path, targetpath)
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
        
        
def find_template_dir(directory=''):
    import couchapp
    default_locations = [
            os.path.join(couchapp.__path__[0], 'templates', directory),
            os.path.join(couchapp.__path__[0], '../templates', directory)
    ]
    
    if directory:
        user_locations = []
        for user_location in user_path():
            user_locations.append(os.path.join(user_location, 'templates', directory))
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