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

from couchapp.http import save_docs
from couchapp.errors import *

import couchapp.app as app

def init(ui, dest=".", **opts):
    app.document(ui, dest, True)

def push(ui, doc, dest=None, **opts):
    localdoc = app.document(ui, doc, True)
    if opts.get('export', False):
        if opts.get('output') is not None:
            self.ui.write_json(kwargs.get('output'), str(localdoc))
        return str(localdoc)
    
    dburls = self.ui.get_db(dest)
    localdoc.push(dburls, opts.get('no_atomic', False))
    docsdir = os.path.join(doc.docdir, '_docs')
    if os.path.exists(docsdir):
        pushdocs(ui, docsdir, dburls, **opts)
    return 0

def pushapps(ui, source, dest, **opts):
    export = opts.get('export', False)
    noatomic = opts.get('no_atomic', False)
    dburls = self.ui.get_db(dest)
    apps = []
    for d in os.listdir(source):
        appdir = os.path.join(source, d)
        if os.path.isdir(appdir) and os.path.isfile(os.path.join(appdir, '.couchapprc')):
            doc = app.document(ui, appdir, True)
            if export or not noatomic:
                apps.append(doc)
            else:
                doc.push(dburls, True)
    if apps:
        if export:
            docs = []
            docs.append([app.doc() for app in apps])
            jsonobj = {'docs': docs}
            if opts.get('output') is not None:
                self.ui.write_json(kwargs.get('output'), json.dumps(jsonobj))
            else:
                print json.dumps(jsonobj)
        else:
            for dburl in dburls:
                docs = []
                docs.append([app.doc(dburl) for app in apps])
                save_docs(dburl, docs)
    return 0
  
def pushdocs(ui, source, dest, **opts):
    export = opts.get('export', False)
    noatomic = opts.get('no_atomic', False)
    dburls = self.ui.get_db(dest)
    docs = []
    for d in os.listdir(source):
        docdir = os.path.join(source, d)
        if docdir.startswith('.'):
            continue
        elif os.path.isfile(docdir):
            if d.endswith(".json"):
                doc = self.ui.read_json(docdir)
                docid, ext = os.path.splitext(d)
                
                doc.setdefault('_id', docid)
                doc.setdefault('couchapp', {})
                if export or not noatomic:
                    docs.append(doc)
                else:
                    for dburl in dburls:
                        save_doc(dburls, doc['_id'])
        else:
            doc = app.document(ui, appdir, True)
            if export or not atomic:
                docs.append(doc)
            else:
                doc.push(dburls, True)
    if docs:
        if export:
            docs1 = []
            for doc in docs:
                if hasattr(doc, 'doc'):
                    docs1.append(doc.doc())
                else:
                    docs1.append(doc)
            jsonobj = {'docs': docs}
            if opts.get('output') is not None:
                self.ui.write_json(kwargs.get('output'), json.dumps(jsonobj))
            else:
                print json.dumps(jsonobj)
        else:
            for dburl in dburls:
                docs1 = []
                for doc in docs:
                    if hasattr(doc, 'doc'):
                        docs1.append(doc.doc(dburl))
                    else:
                        docs1.append(doc)
                save_docs(dburl, docs1)
    return 0
    
def clone(ui, source, dest=".", **opts):
    app.clone(ui, source, dest, rev=opts.get('rev'))
    return 0

def generate(ui, kind="app", dest=".", *args, **opts):
    if kind != "app" and len(args) < 1:
        raise AppError("name is missing")
    app.generate(ui, dest, kind, *args)
    return 0
    
def vendor(ui, cmd, dest=".", *args, **opts):
    if cmd == "install":
        if len(args) < 1:
            raise AppError("missing source")
        app.vendor_install(ui, dest, args[1])
    else:
        app.vendor_update(ui, dest, args)
    return 0
    
    
globalopts = [
    ('-v', 'verbose', 'store_const', 1, 2, "enable additionnal output"),
    ('-q', 'verbose', 'store_const', 0, 0, "don't print any message")
]

pushopts = [
    ('--no-atomic', 'noatomic', 'store_true', False, None, "Send attachments one by one"),
    ('--export', 'export', 'store_true', False, None, "don't do push, just export doc to stdout"),
    ('--output', 'output', 'store', None, None, "if export is selected, output to the file")
]
    
table = {
    "init": 
        (init, 
        [], 
        "[DEST]"),
    "push":
        (push,
        pushopts,
        "[OPTION]... [SOURCE] [DEST]"),
    "clone":
        (clone,
        [('-r', 'rev', 'store', None, None, "clone specific revision")],
        "[OPTION]...[-r REV] [SOURCE] [DEST]"),
    "pushapps":
        (pushapps,
        pushopts,
        "[OPTION]... [SOURCE] [DEST]"),
    "pushdocs":
        (pushdocs,
        pushopts,
        "[OPTION]... [SOURCE] [DEST]"),
    "generate":
        (generate,
        [],
        "[OPTION]... [app|view,list,show,filter,function,vendor] [DEST] NAME [TEMPLATE]"),
    "vendor":
        (vendor,
        [],
        "[OPTION]... install|update [DEST] [SOURCE]")
}

withcmd = ['generate', 'vendor']
incouchapp = ['init', 'push', 'generate', 'vendor']