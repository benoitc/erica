#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# Copyright 2008, 2009 Benoit Chesneau <benoitc@e-engura.org>
#
# This software is licensed as described in the file LICENSE, which
# you should have received as part of this distribution.

from optparse import OptionParser
import os
import sys

from couchapp.config import get_config

sys.path.append(os.path.dirname(__file__))

def merge_js(app_dir, app_name, verbose=False):
    docid = '_design/%s' % app_name
    attach_dir = os.path.join(app_dir, '_attachments')
    
    conf = get_config(app_dir)
    if "js_compressor" in conf:
        if not isinstance(conf["js_compressor"], basestring):
            print >>sys.stderr, "Warning: js_compressor settings should be a string"
            print >>sys.stderr, "         Selecting default backend (jsmin)"
            import couchapp.utils.jsmin as backend
        else:
            try:
                backend = __import__(conf['js_compressor'], {}, {}, [''])
            except ImportError:
                import couchapp.utils.jsmin as backend
    else:
        import couchapp.utils.jsmin as backend

    if verbose:
        backend.about()

    for fname, src_files in conf['js'].iteritems():
        output_js = ''

        dest_path = os.path.join(attach_dir, fname)
        fname_dir = os.path.dirname(dest_path)

        for src_fname in src_files:
            src_fpath = os.path.join(attach_dir, src_fname)
            if os.path.isfile(src_fpath):
                output_js += "/* %s */\n" % src_fpath
                output_js +=  read_file(src_fpath)
                if verbose >= 2:
                    print "merging %s in %s" % (src_fname, fname)

        if not os.path.isdir(fname_dir):
            os.makedirs(fname_dir)

        output_js = backend.compress(output_js)
        write_content(dest_path, output_js)
        
def main():
    parser = OptionParser(usage='%prog appdir appname') 
    options, args = parser.parse_args()

    if len(args) < 2:
        return parser.error('incorrect number of arguments')
        
    merge_js(args[0], args[1])
    
if __name__ == '__main__':
    main()