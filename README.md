#Erica

## Made couchapp development easy
 
Erica is a tool compatible with
[couchapp](http://github.com/couchapp/couchapp) that helps you to create
and manage your couchapps (CouchDB embedded applications).

##Requirements

* Erlang R13B04 or sup
* gcc

##Installation

Couchapp requires Erlang R13B04 or sup.

    $ git clone git://github.com/benoitc/erica.git
    $ cd erica
    $ make

Then you can use the generated script **erica** .

To install it on your system, run the command line:

    $ make install

Usage:

    $ erica -h
    Usage: erica [-h] [-c] [-v] [-f] [-V] [--is-ddoc <is_ddoc>] [--docid <docid>] [--atomic <atomic>] [...] <command,...>

      -h, --help		Show the program options
      -c, --commands	Show available commands
      -v, --verbose		Be verbose about what gets done
      -f, --force		Force
      -V, --version		Show version information
      --is-ddoc		Tell to push command if you send a design document or not.
      --docid		Set docid with push command
      --atomic		Send attachments inline with push command
      command		Command to run (e.g. push)

Available commands are:

    init                                 initialize a erica
    push        [options...] [dir] dest  push a document to couchdb
    clone       [option] source dir      clone a document from couchdb
    browse                               display the erica in the
                                         browser.
    create-app  appid=AppID lang=Lang    Create a blank couchapp, Default:
                                         appid=myapp, lang=javascript
    create      template= [vars...]      create an application using a
                                         template
    help                                 Show the program options
    version                              Show version information
    

## First steps
