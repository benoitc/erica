# -*- coding: utf-8 -*-
#
# Copyright 2009 Benoit Chesneau <benoitc@e-engura.org>
#
# This software is licensed as described in the file LICENSE, which
# you should have received as part of this distribution.
#

import os
import sys

from couchapp.utils import read_json

external_dir = os.path.join(os.path.dirname(__file__), '_external')

def get_userconf():
    """ return user conf from ~/.couchapprc 
    
    :return: dict
    """
    # this should work on windows too
    homedir = os.path.expanduser('~')
    user_conffile = os.path.join(homedir, ".couchapprc")
    if os.path.isfile(user_conffile):
        try:
            return read_json(user_conffile, use_environment=True)
        except:
            pass
    return {}


def get_config(app_dir=None):
    """ Get current configuration of couchapp. If app_dir is given
     it return user configuration and local app configuration.
     
     :attr app_dir: string, path of application
     
     :return: dict, configuratuib
    """
    conf = get_userconf()
    if app_dir and app_dir is not None:
        rc_file = os.path.join(app_dir, '.couchapprc')
        if os.path.isfile(rc_file):
            conf.update(read_json(rc_file, use_environment=True))
    return conf
    
    
