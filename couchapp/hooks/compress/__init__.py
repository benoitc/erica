# -*- coding: utf-8 -*-
#
# This file is part of couchapp released under the Apache 2 license. 
# See the NOTICE for more information.

import logging
import os
import re

from couchapp.config import Config
from couchapp.hooks.compress import compress_css
from couchapp import util

logger = logging.getLogger(__name__)

class Compress(object):
    
    def __init__(self, path):
        self.appdir = path
        self.attach_dir = os.path.join(path, '_attachments')
        self.conf = Config()
        self.conf.update(path)
        
    def is_hook(self):
        if not 'compress' in self.conf:
            return False
        return True
        
    def compress_css(self, css):
        re_url = re.compile('url\s*\(([^\s"].*)\)')

        src_fpath = ''
        fname_dir = ''

        def replace_url(mo):
            """ make sure urls are relative to css path """
            css_url = mo.group(0)[4:].strip(")").replace("'", "").replace('"','')
            css_path = os.path.join(os.path.dirname(src_fpath),
                    css_url)

            rel_path = util.relpath(css_path, fname_dir)
            return "url(%s)" % rel_path

        for fname, src_files in css.iteritems():
            output_css = ''

            dest_path = os.path.join(self.attach_dir, fname)
            fname_dir = os.path.dirname(dest_path)

            for src_fname in src_files:
                src_fpath = os.path.join(self.appdir, src_fname)

                if os.path.exists(src_fpath):
                    content_css = str(compress_css.CSSParser(
                                                        util.read(src_fpath)))
                    content_css = re_url.sub(replace_url, content_css)
                    output_css += content_css
                    logger.debug("Merging %s in %s" % (src_fname, fname))

            if not os.path.isdir(fname_dir):
                os.makedirs(fname_dir)
            util.write(dest_path, output_css)
            
    def compress_js(self, backend, js):
        logger.info("compress js with %s " % backend.__about__)

        for fname, src_files in js.iteritems():
            output_js = ''

            dest_path = os.path.join(self.attach_dir, fname)
            fname_dir = os.path.dirname(dest_path)

            for src_fname in src_files:
                src_fpath = os.path.join(self.appdir, src_fname)
                if os.path.isfile(src_fpath):
                    output_js += "/* %s */\n" % src_fpath
                    output_js +=  util.read(src_fpath)
                    logger.debug("merging %s in %s" % (src_fname, fname))

            if not os.path.isdir(fname_dir):
                os.makedirs(fname_dir)

            output_js = backend.compress(output_js)
            util.write(dest_path, output_js)
        
    def run(self):
        conf = self.conf
        actions = conf.get('compress', {})
        if 'css' in actions:
            self.compress_css(actions['css'])
        
        if 'js' in actions:
            if 'js_compressor' in conf['compress']:
                modname = conf['compress']['js_compressor']
                if not isinstance(modname, basestring):
                    logger.warning("Warning: js_compressor settings should be a string")
                    logger.warning("Selecting default backend (jsmin)")
                    import couchappext.compress.jsmin as backend
                else:
                    try:
                        backend = __import__(modname, {}, {}, [''])
                    except ImportError:
                         import couchappext.compress.jsmin as backend
            else:
                 import couchappext.compress.jsmin as backend
            self.compress_js(backend, actions['js'])
        

def hook(path, hooktype, **kwarg):
    c = Compress(path)
        
    if hooktype == "pre-push":
        if not c.is_hook(): return
        c.run()
    
