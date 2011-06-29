#Erica

## Made couchapp development easy
 
Erica is a tool compatible with
[couchapp](http://github.com/couchapp/couchapp) that helps you to create
and manage your couchapps (CouchDB embedded applications).

##Requirements

* Erlang R13B04 or sup
* gcc

##Installation

Couchapp requires Erlang Erlang R13B04 (or higher) with crypto support.

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
    
Provided templates are for now:

* simpleapp (the default create used for create-app command)
* example: a simple couchapp exampole
* couchapp: a simple template with the good old couchapp javascript
  library.

You can add your own template in ~/.erica/templates.

## First steps

Here we will show you how to create your first application and main
usages of commands.

### 1 . create your first application

You can use a template to create your first application. Like the
generic one. It will create a simple project that you can use for a
start:

    $ erica create-app appid=myapp lang=javascript
    ==> tmp (create-app)
    Writing myapp/_id
    Writing myapp/language
    Writing myapp/.couchapprc

    $ ls -fla myapp/
    total 24
    drwxr-xr-x   6 benoitc  wheel  204 Jun  7 11:13 .
    drwxrwxrwt  13 root     wheel  442 Jun  7 11:13 ..
    -rw-r--r--   1 benoitc  wheel   18 Jun  7 11:13 .couchapprc
    drwxr-xr-x   2 benoitc  wheel   68 Jun  7 11:13 _attachments
    -rw-r--r--   1 benoitc  wheel   13 Jun  7 11:13 _id
    -rw-r--r--   1 benoitc  wheel   10 Jun  7 11:13 language

Erica has created an _attachments folder in the myapp folder. This is
where you can put all the attachments. You can put your views functions in
`views/viewname/{map,reduce}.js` , shows in `shows` folder, lists in
`lists`, ... See the wiki for more info (soon).

* *_id* is where you set the document id
* *languages* is where you set the language of your application
* *.couchapprc* is where you set some config infos for your app.
    
Note: **erica** is language agnostic, so if you want to create your couchapp in
coffescript, just replace javascript by coffescript or yuse the language
you want if an couchapp server exists for it.

### 2. Let's create a simple hello world

Add an index.html in your attachments folder:

    $ cd myapp
    $ echo "hello world" > _attachments/index.html

Then push it to your couchdb node

    $ erica push http://127.0.0.1:5984/testdb

Then visit the result on

    http://127.0.0.1:5984/testdb/_design/myapp/index.html

That's it.

Note: By default the CouchDB Node uri is 127.0.0.1:5984 so you could
just use the db name in push command line if you want:

    $ erica push testdb

This is fully configurable in your .couchapprc.

### 3. Clone

Did you see an interresting couchapp you want to reuse? Or just working
with a friend on the same couchapp ? With the `clone` command you can
replicate a couchapp on your filesystem, edit it and push the results
after:

    $ erica clone http://127.0.0.1:5984/testdb/_design/myapp 

This command will clone the couchapp `myapp` in the `myapp` folder. If
you want to clone it to another folder, just do:

    $ erica clone http://127.0.0.1:5984/testdb/_design/myapp mynewapp

## More

If you have any question contact us on irc freenode **#couchapp** or on
the mailing-list: http://groups.google.com/group/couchapp .


    
