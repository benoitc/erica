#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# Copyright 2008, 2009 Benoit Chesneau <benoitc@e-engura.org>
#
# Code taked from Friendpaste under license:
# Apache 2 License
#
# 2008 Benoit Chesneau <benoitc@e-engura.org>
#
# This software is licensed as described in the file LICENSE, which
# you should have received as part of this distribution.
#

import re

__all__ = ['CSSParser']

re_selector = re.compile("/\./")
re_comments = re.compile("(\/\*).*?(\*\/)")
re_sep = re.compile(':\s*')
re_line1 = re.compile('\n')
re_line = re.compile('(\n)')
re_comma = re.compile(',')
re_comma2 = re.compile(',  ')


def strip_space(string):
    """  strip space after :, remove newlines, replace 
    multiple spaces with only one space, remove comments """
    if isinstance(string, basestring):
        string = re_line1.sub('', re_sep.sub(':', string))
        string = re_comments.sub('', string.strip())
    return string
    
def strip_selector_space(string):
    """ remove newlines, insert space after comma, replace 
    two spaces with one space after comma """
    if isinstance(string, basestring):
        string = re_comma2.sub(', ', re_comma.sub(', ', re_line.sub('', string)))
    return string
    
class CSSParser(object):
    
    def __init__(self, css_string, options=None):
        options = options or {}
        self.namespace = options.get('namespace', '')
        self.raw_data = css_string
        self.css_output = ''
        
        self._compress(self.raw_data)
        
    def __str__(self):
        return self.css_output
        
    def parse(self, data):
        data = data or self.raw_data
        css_out = []
        data = strip_space(data.strip())
        for index, assignements in enumerate(data.split('}')):
            try:
                tags, styles = [a.strip() for a in assignements.strip().split('{')]
                rules = []
                if styles:
                    if self.namespace:
                        tags = strip_selector_space(tags)
                        tags = re_selector.sub(self.namespace, tags)
                        rules = []
                    for key_val_pair in styles.split(';'):
                        try:
                            key, value =  [a.strip() for a in key_val_pair.split(':')]
                            rules.append("%s:%s;" % (key, value))
                        except ValueError:
                            continue
                    css_out.append({
                        'tags': tags,
                        'rules': ''.join(rules),
                        'idx': index 
                    })
            except ValueError:
                continue
        css_out.sort(lambda a,b: cmp(a['idx'], b['idx']))
        return css_out
        
    def _compress(self, data):
        self.css_output = '';

        for line in self.parse(data):
            self.css_output += "%s {%s}\n" % (line['tags'], line['rules'])    
