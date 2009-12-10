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
try:
    import json
except ImportError:
    import couchapp.simplejson as json

import couchapp.app as app
from couchapp.errors import ResourceNotFound, AppError
from couchapp.extensions import get_extensions, load_extensions
from couchapp import hooks

def _maybe_reload(ui, path, new_path):
    if path is None:
        # we reload extensions and updaye confing
        ui.updateconfig(new_path)
        load_extensions(ui)

def init(ui, path, *args, **opts):
    if not args:
        dest = path
    else:
        dest = os.path.normpath(os.path.join(os.getcwd(), args[0]))
        
    if dest is None:
        raise AppError("You aren't in a couchapp.")
    app.document(ui, dest, True)

def push(ui, path, *args, **opts):
    export = opts.get('export', False)
    dest = None
    doc_path = None
    if len(args) < 2:
        if export:
            if path is None and args:
                doc_path = args[0]
            else:
                doc_path = path
        else:
            doc_path = path
            if args:
                dest = args[0]
    else:
        doc_path = os.path.normpath(os.path.join(os.getcwd(), args[0]))
        dest = args[1]
    if doc_path is None:
        raise AppError("You aren't in a couchapp.")
    
    _maybe_reload(ui, path, doc_path)
    
    localdoc = app.document(ui, doc_path, create=False, docid=opts.get('docid'))
    if export:
        if opts.get('output'):
            ui.write_json(opts.get('output'), str(localdoc))
        else:
            print str(localdoc)
        return 0
    dbs = ui.get_dbs(dest)
    hooks.hook(ui, doc_path, "pre-push", dbs=dbs)    
    localdoc.push(dbs, opts.get('no_atomic', False))
    hooks.hook(ui, doc_path, "post-push", dbs=dbs)
    
    docspath = os.path.join(doc_path, '_docs')
    if os.path.exists(docspath):
        pushdocs(ui, docspath, dest, *args, **opts)
    return 0

def pushapps(ui, source, dest, *args, **opts):
    export = opts.get('export', False)
    noatomic = opts.get('no_atomic', False)
    dbs = ui.get_dbs(dest)
    apps = []
    source = os.path.normpath(os.path.join(os.getcwd(), source))
    for d in os.listdir(source):
        appdir = os.path.join(source, d)
        print appdir
        if os.path.isdir(appdir) and os.path.isfile(os.path.join(appdir, '.couchapprc')):
            localdoc = app.document(ui, appdir)
            hooks.hook(ui, appdir, "pre-push", dbs=dbs, pushapps=True)
            if export or not noatomic:
                apps.append(localdoc)
            else:
                localdoc.push(dbs, True)
            hooks.hook(ui, appdir, "post-push", dbs=dbs, pushapps=True)
    if apps:
        if export:
            docs = []
            docs.append([localdoc.doc() for localdoc in apps])
            jsonobj = {'docs': docs}
            if opts.get('output') is not None:
                ui.write_json(opts.get('output'), json.dumps(jsonobj))
            else:
                print json.dumps(jsonobj)
            return 0
        else:
            for db in dbs:
                docs = []
                docs = [doc.doc(db) for doc in apps]
                db.save_docs(docs)
    return 0
  
def pushdocs(ui, source, dest, *args, **opts):
    export = opts.get('export', False)
    noatomic = opts.get('no_atomic', False)
    dbs = ui.get_dbs(dest)
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
                    for db in dbs:
                        db.save_doc(doc)
        else:
            doc = app.document(ui, docdir)
            if export or not noatomic:
                docs.append(doc)
            else:
                doc.push(dbs, True)
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
                ui.write_json(opts.get('output'), json.dumps(jsonobj))
            else:
                print json.dumps(jsonobj)
        else:
            for db in dbs:
                docs1 = []
                for doc in docs:
                    if hasattr(doc, 'doc'):
                        docs1.append(doc.doc(db))
                    else:
                        newdoc = doc.copy()
                        try:
                            olddoc = db.get_doc(doc['_id'])
                            newdoc.update({'_rev': olddoc['_rev']})
                        except ResourceNotFound:
                            pass
                        docs1.append(newdoc)
                db.save_docs(docs1)
    return 0
    
def clone(ui, source, *args, **opts):
    if len(args) > 0:
        dest = args[0]
    else:
        dest = None 
    hooks.hook(ui, dest, "pre-clone", source=source)
    app.clone(ui, source, dest, rev=opts.get('rev'))
    hooks.hook(ui, dest, "post-clone", source=source)
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
    
    hooks.hook(ui, dest, "pre-generate")    
    app.generate(ui, dest, kind, name, **opts)
    _maybe_reload(ui, path, dest)
    hooks.hook(ui, dest, "post-generate")
       
    return 0
    
def vendor(ui, path, *args, **opts):
    if len(args) < 1:
        raise AppError("missing command")
    dest = path
    args = list(args)
    cmd = args.pop(0)
    if cmd == "install":
        if len(args) < 1:
            raise AppError("missing source")
        if len(args) == 1:
            source = args.pop(0)
            
        elif len(args) > 1:
            dest = args.pop(0)
            source = args.pop(1)
        
        if dest is None:
            raise AppError("You aren't in a couchapp.")
            
        dest = os.path.normpath(os.path.join(os.getcwd(), dest))
        _maybe_reload(ui, path, dest)
        hooks.hook(ui, dest, "pre-vendor", source=source, action="install")
        app.vendor_install(ui, dest, source, *args, **opts)
        hooks.hook(ui, dest, "post-vendor", source=source, action="install")
    else:
        vendorname = None
        if len(args) == 1:
            vendorname=args.pop(0)
        elif len(args) >= 2:
            dest = args.pop(0)
            vendorname=args.pop(1)
        if dest is None:
            raise AppError("You aren't in a couchapp.")
            
        dest = os.path.normpath(os.path.join(os.getcwd(), dest))
        hooks.hook(ui, dest, "pre-vendor", name=vendorname, action="update")
        app.vendor_update(ui, dest, vendorname, *args, **opts)
        hooks.hook(ui, dest, "pre-vendor", name=vendorname, action="update")
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
    print "-----------------"
    print ""
    for cmd in sorted(table.keys()):
        opts = table[cmd]
        print "%s\t %s" % (cmd, opts[2])
        for opt in opts[1]:
            print_option(opt)
        print ""
    print "loaded extensions:"
    print "------------------"
    print ""
    for name, mod in get_extensions():
        name = getattr(mod, '__extension_name__', name)
        print "%s" % name
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
    ('', 'no-atomic', False, "send attachments one by one"),
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
        pushopts + [('', 'docid', '', "set docid")],
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
        [("f", 'force', False, "force install or update")],
        "[OPTION]...[-f] install|update [COUCHAPPDIR] SOURCE"),
    "help":
        (usage, [], ""),
    "version":
        (version, [], "")
}

withcmd = ['generate', 'vendor']
incouchapp = ['init', 'push', 'generate', 'vendor']
