# -*- coding: utf-8 -*-
#
# This file is part of couchapp released under the Apache 2 license. 
# See the NOTICE for more information.

from __future__ import with_statement

import errno
import fcntl
import logging
import os
import re
import select
import signal
import sys
import time
import traceback

from couchapp.errors import AppError
from couchapp.localdoc import document
from couchapp.util import json

try:
    import pyinotify
except ImportError:
    pyinotify = None

try:
    import eventlet
    eventlet.monkey_patch(os=False)
except ImportError:
    raise AppError("""Eventlet isn't installed.

Install it with the command:
        
    pip install eventlet
""")


log = logging.getLogger(__name__)


def close_on_exec(fd):
    flags = fcntl.fcntl(fd, fcntl.F_GETFD)
    flags |= fcntl.FD_CLOEXEC
    fcntl.fcntl(fd, fcntl.F_SETFD, flags)
    
def set_non_blocking(fd):
    flags = fcntl.fcntl(fd, fcntl.F_GETFL) | os.O_NONBLOCK
    fcntl.fcntl(fd, fcntl.F_SETFL, flags)

def check_ignore(ignores, item):
    for i in ignores:
        match = re.match(i, item)
        if match:
            log.debug("ignoring %s" % item)
            return True
    return False


class CouchappWatcher(object):

    UPDATES = []
  
    PIPE = []

    # I love dynamic languages
    SIG_QUEUE = []
    SIGNALS = map(
        lambda x: getattr(signal, "SIG%s" % x),
        "HUP QUIT INT TERM TTIN TTOU USR2".split()
    )
    SIG_NAMES = dict(
        (getattr(signal, name), name[3:].lower()) for name in dir(signal)
        if name[:3] == "SIG" and name[3] != "_"
    )

    def __init__(self, conf, path, *args, **opts):
        # initial vars
        self.conf = conf
        self.path = path
        self.args = args
        self.opts = opts


         # init
        self.doc = None
        self.dbs = []
        self.noatomic = False
        self.ignores = []

        self.setup()


    def setup(self):
        self.noatomic = self.opts.get('no_atomic', False)
        self.update_delay = self.opts.get('update_delay', 60)
        dest = None
        doc_path = None
        if len(self.args) < 2:
            doc_path = self.path
            if self.args:
                dest = self.args[0]
        else:
            doc_path = os.path.normpath(os.path.join(os.getcwd(), self.args[0]))
            dest = self.args[1]

        if doc_path is None:
            raise AppError("You aren't in a couchapp.")


        self.conf.update(doc_path)

        self.doc_path = doc_path
        self.doc = document(doc_path, create=False, 
                        docid=self.opts.get('docid'))

        self.dbs = self.conf.get_dbs(dest)
       
        ignorefile = os.path.join(self.doc_path, '.couchappignore')
        if os.path.exists(ignorefile):
            # A .couchappignore file is a json file containing a
            # list of regexps for things to skip
            self.ignores = json.load(open(ignorefile, 'r'))


    def start(self):
        """\
        Initialize the arbiter. Start listening and set pidfile if needed.
        """
        self.pid = os.getpid()
        self.init_signals()
        log.info("Autopush handler started [%s]" % self.pid)

    def init_signals(self):
        """\
        Initialize master signal handling. Most of the signals
        are queued. Child signals only wake up the master.
        """
        if self.PIPE:
            map(lambda p: os.close(p), self.PIPE)
        self.PIPE = pair = os.pipe()
        map(set_non_blocking, pair)
        map(close_on_exec, pair)
        map(lambda s: signal.signal(s, self.signal), self.SIGNALS)
        signal.signal(signal.SIGCHLD, self.handle_chld)
    
    def signal(self, sig, frame):
        if len(self.SIG_QUEUE) < 5:
            self.SIG_QUEUE.append(sig)
            self.wakeup()
        else:
            log.warn("Dropping signal: %s" % sig)
 

    def update(self):
        nb_changes = 0
        for idx, update_time in enumerate(self.UPDATES):
            diff = time.time() - update_time
            if diff <= self.update_delay:
                break
            nb_changes += 1
            del self.UPDATES[idx]
    
        if nb_changes > 0:
            log.info("Push change")
            self.doc.push(self.dbs, noatomic=self.noatomic, noindex=True)
    
    def scan(self):
        pass

    def run(self):
        self.start()
        while True:
            try:
                sig = self.SIG_QUEUE.pop(0) if len(self.SIG_QUEUE) else None
                if sig is None:
                    self.scan()
                    eventlet.spawn_n(self.update)
                    eventlet.sleep(0.1)
                    self.sleep()
                    continue
                if sig not in self.SIG_NAMES:
                    self.log.info("Ignoring unknown signal: %s" % sig)
                    continue

                signame = self.SIG_NAMES.get(sig)
                handler = getattr(self, "handle_%s" % signame, None)
                if not handler:
                    log.error("Unhandled signal: %s" % signame)
                    continue
                log.info("handling signal: %s" % signame)
                handler()
                self.wakeup()
                eventlet.sleep(0.1)
            except StopIteration:
                self.halt()
            except KeyboardInterrupt:
                self.halt()
            except Exception, e:
                log.info("unhandled exception in main loop:\n%s" %
                        traceback.format_exc())
                sys.exit(-1)

    def handle_chld(self, sig, frame):
        return
        self.wakeup()

    def handle_hup(self):
        log.info("Hang up")
        self.setup()

    def handle_quit(self):
        raise StopIteration

    def handle_int(self):
        raise StopIteration

    def handle_term(self):
        raise StopIteration

    def wakeup(self):
        try:
            os.write(self.PIPE[1], '.')
        except IOError, e:
            if e.errno not in [errno.EAGAIN, errno.EINTR]:
                raise
    
    def halt(self):
        self.stop()
        sys.exit(0)

    def sleep(self):
        try:
            ready = select.select([self.PIPE[0]], [], [], 1.0)
            if not ready[0]:
                return
            while os.read(self.PIPE[0], 1):
                pass
        except select.error, e:
            if e[0] not in [errno.EAGAIN, errno.EINTR]:
                raise
        except OSError, e:
            if e.errno not in [errno.EAGAIN, errno.EINTR]:
                raise
        except KeyboardInterrupt:
            sys.exit(0)

    def stop(self):
        self.update()


class PollWatcher(CouchappWatcher):

    def start(self):
        super(PollWatcher, self).start()
        self.all_files = {}

    def scan(self):
        # Basic principle: all_files is a dictionary mapping paths to
        # modification times.  We repeatedly crawl through the directory
        # tree rooted at 'path', doing a stat() on each file and comparing
        # the modification time.

        def f (unused, dirname, files):
            # Traversal function for directories
            for filename in files:
                path = os.path.join(dirname, filename)
                if check_ignore(self.ignores, path):
                    continue

                try:
                    t = os.stat(path)
                except os.error:
                    # If a file has been deleted between os.path.walk()
                    # scanning the directory and now, we'll get an
                    # os.error here.  Just ignore it -- we'll report
                    # the deletion on the next pass through the main loop.
                    continue

                mtime = remaining_files.get(path)
                if mtime is not None:
                    # Record this file as having been seen
                    del remaining_files[path]
                    # File's mtime has been changed since we last looked at it.
                    if t.st_mtime > mtime:
                        changed_list.append(path)
                else:
                    # No recorded modification time, so it must be
                    # a brand new file.
                    changed_list.append(path)

                # Record current mtime of file.
                self.all_files[path] = t.st_mtime

        changed_list = []
        remaining_files = self.all_files.copy()
        
        self.all_files = {}
        os.path.walk(self.doc_path, f, None)
        removed_list = remaining_files.keys()
        if changed_list or removed_list:
            self.UPDATES.append(time.time())


if pyinotify is not None:
    class AutopushHandler(pyinotify.ProcessEvent):

        def my_init(self, watcher=None):
            self.watcher = watcher

        def process_default(self, event):
            if check_ignore(self.watcher.ignores, event.pathname):
                return
            self.watcher.UPDATES.append(time.time())

    class INotifyWatcher(CouchappWatcher):

        def start(self):
            super(INotifyWatcher, self).start()
            self.wm = pyinotify.WatchManager()
            mask = pyinotify.IN_MOVED_TO | pyinotify.IN_MOVED_FROM | \
                    pyinotify.IN_MODIFY | pyinotify.IN_CREATE | \
                    pyinotify.IN_DELETE | pyinotify.IN_DELETE_SELF | \
                    pyinotify.IN_ATTRIB
            self.notifier = pyinotify.Notifier(self.wm,
                    AutopushHandler(watcher=self))
            self.wm.add_watch(self.doc_path, mask, rec=True, auto_add=True)

        def scan(self):
            try:
                ready = select.select([self.notifier._fd], [], [], 1.0)
                if not ready[0]:
                    return
                self.notifier.read_events()
                self.notifier.process_events()

            except select.error, e:
                if e[0] not in [errno.EAGAIN, errno.EINTR]:
                    raise
            except KeyboardInterrupt:
                sys.exit(0)


        def stop(self):
            self.notifier.stop()
            super(INotifyWatcher, self).stop()



def autopush(conf, path, *args, **opts):
    no_inotify = opts.get('no_inotify', False)

    if pyinotify is not None and not no_inotify:
        watcher_class = INotifyWatcher
    else:
        watcher_class = PollWatcher

    watcher = watcher_class(conf, path, *args, **opts)
    watcher.run()


cmdtable = {
    "autopush":
        (autopush,
        [('', 'no-atomic', False, "send attachments one by one"),
        ('', 'no-inotify', False, "Don't use inotify api"),
        ('', 'update-delay', 60, "time between each update")],
        "[OPTION]... [COUCHAPPDIR] DEST")
}
