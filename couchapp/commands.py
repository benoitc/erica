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

from couchapp.http import save_docs
from couchapp.errors import *

import couchapp.app as app

def init(ui, path, *args, **opts):
    if not args:
        dest = path
    else:
        dest = os.path.normpath(os.path.join(os.getcwd(), args[0]))
        
    if dest is None:
        raise AppError("You aren't in a couchapp.")
    app.document(ui, dest, True)

def push(ui, path, *args, **opts):
    if len(args) < 2:
        doc_path = path
        dest = args[0]
    else:
        doc_path = os.path.normpath(os.path.join(os.getcwd(), args[0]))
        dest = args[1]
    if doc_path is None:
        raise AppError("You aren't in a couchapp.")
    
    localdoc = app.document(ui, doc_path, False)
    if opts.get('export', False):
        if opts.get('output') is not None:
            ui.write_json(kwargs.get('output'), str(localdoc))
        return str(localdoc)
    
    dburls = ui.get_dbs(dest)
    localdoc.push(dburls, opts.get('no_atomic', False))
    docsdir = os.path.join(localdoc.docdir, '_docs')
    if os.path.exists(docsdir):
        pushdocs(ui, docsdir, dburls, **opts)
    return 0

def pushapps(ui, source, dest, *args, **opts):
    export = opts.get('export', False)
    noatomic = opts.get('no_atomic', False)
    dburls = ui.get_dbs(dest)
    apps = []
    for d in os.listdir(source):
        appdir = os.path.join(source, d)
        if os.path.isdir(appdir) and os.path.isfile(os.path.join(appdir, '.couchapprc')):
            localdoc = app.document(ui, appdir)
            if export or not noatomic:
                apps.append(localdoc)
            else:
                localdoc.push(dburls, True)
    if apps:
        if export:
            docs = []
            docs.append([localdoc.doc() for localdoc in apps])
            jsonobj = {'docs': docs}
            if opts.get('output') is not None:
                ui.write_json(kwargs.get('output'), json.dumps(jsonobj))
            else:
                print json.dumps(jsonobj)
        else:
            for dburl in dburls:
                docs = []
                docs = [doc.doc(dburl) for doc in apps]
                save_docs(dburl, docs)
                
    return 0
  
def pushdocs(ui, source, dest, *args, **opts):
    export = opts.get('export', False)
    noatomic = opts.get('no_atomic', False)
    dburls = ui.get_dbs(dest)
    docs = []
    for d in os.listdir(source):
        docdir = os.path.join(source, d)
        if docdir.startswith('.'):
            continue
        elif os.path.isfile(docdir):
            if d.endswith(".json"):
                doc = ui.read_json(docdir)
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
                ui.write_json(kwargs.get('output'), json.dumps(jsonobj))
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
    
def clone(ui, source, *args, **opts):
    if len(args) > 1:
        dest = args[0]
    else:
        dest = "."
        
    
    app.clone(ui, source, dest, rev=opts.get('rev'))
    return 0

def generate(ui, path, *args, **opts):
    dest = path
    if len(args) < 1:
        raise AppError("Can't generate function, name or pat is missing")
        
    if len(args) == 1:
        kind="app"
        name = args[0]
    elif len(args) == 2:
        kind = args[0]
        name = args[1]
    elif len(args) >= 3:
        kind = args[0]
        dest = args[1]
        name = args[2]
        
    if dest is None:
        if kind == "app":
            dest = os.path.normpath(os.path.join(os.getcwd(), ".", name))
            opts['create'] = True
        else:
            raise AppError("You aren't in a couchapp.")
        
    app.generate(ui, dest, kind, name, **opts)
    return 0
    
def vendor(ui, path, *args, **opts):
    if len(args) < 1:
        raise AppError("missing command")
    dest = path
    cmd = args.pop(0)
    if cmd == "install":
        if len(args) < 1:
            raise AppError("missing source")
        if len(args) == 1:
            souce = args[0]
            
        elif len(args) >= 1:
            dest = args[0]
            source = args[1]
        
        if dest is None:
            raise AppError("You aren't in a couchapp.")
        app.vendor_install(ui, dest, source)
    else:
        vendorname = None
        if len(args) == 1:
            vendorname=args[0]
        elif len(args) >= 2:
            dest = args[0]
            vendorname=args[0]
        if dest is None:
            raise AppError("You aren't in a couchapp.")
        app.vendor_update(ui, dest, vendorname)
    return 0
   
def version(ui, *args, **opts):
    from couchapp import __version__
    
    print "Couchapp (version %s)" % __version__
    print "Copyright 2008,2009 Benoît Chesneau <benoitc@e-engura.org>"
    print "Licensed under the Apache License, Version 2.0." 
    print ""
    if opts.get('help', False):
        usage(ui, *args, **opts)
    
    return 0
    
def usage(ui, *args, **opts):
    if opts.get('version', False):
        version(ui, *args, **opts)
    print "couchapp [OPTIONS] [CMD] [OPTIONSCMD] [ARGS,...]"
    print "usage:"
    print ""

    mainopts = []
    for opt in globalopts:
        print_option(opt)
        mainopts.append(opt[0])
        
    print ""
    print "list of commands:"
    print ""
    for cmd, opts in table.items():
        print "%s\t %s" % (cmd, opts[2])
        for opt in opts[1]:
            print_option(opt)
        print ""
    return 0

def print_option(opt):
    if opt[2] is None or opt[2] is True or opt[2] is False:
        default = ""
    else:
        default = " [VAL]"
    if opt[0]:
        print "-%s/--%s%s\t %s" % (opt[0], opt[1], default, opt[3])
    else:
        print "--%s%s\t %s" % (opt[1], default, opt[3])
    
globalopts = [
    ('h', 'help', None, "display help and exit"),
    ('', 'version', None, "display version and exit"),
    ('v', 'verbose', None, "enable additionnal output"),
    ('q', 'quiet', None, "don't print any message")
]

pushopts = [
    ('', 'no-atomic', False, "Send attachments one by one"),
    ('', 'export', False, "don't do push, just export doc to stdout"),
    ('', 'output', '', "if export is selected, output to the file")
]
    
    
    
table = {
    "init": 
        (init, 
        [], 
        "[COUCHAPPDIR]"),
    "push":
        (push,
        pushopts,
        "[OPTION]... [COUCHAPPDIR] DEST"),
    "clone":
        (clone,
        [('r', 'rev', '', "clone specific revision")],
        "[OPTION]...[-r REV] SOURCE [COUCHAPPDIR]"),
    "pushapps":
        (pushapps,
        pushopts,
        "[OPTION]... SOURCE DEST"),
    "pushdocs":
        (pushdocs,
        pushopts,
        "[OPTION]... SOURCE DEST"),
    "generate":
        (generate,
        [('', 'template', '', "template name")],
        "[OPTION]... [app|view,list,show,filter,function,vendor] [COUCHAPPDIR] NAME"),
    "vendor":
        (vendor,
        [],
        "[OPTION]... install|update [COUCHAPPDIR] SOURCE"),
    "help":
        (usage, [], ""),
    "version":
        (version, [], "")
}

withcmd = ['generate', 'vendor']
incouchapp = ['init', 'push', 'generate', 'vendor']