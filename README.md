#Erica

## Making CouchDB development easy

Erica is a tool that helps you to create couchdb design docs,
and web applications (couchapps), and generally get files in and out
of CouchDB.

##Requirements

###Runtime
* Erlang R14B04 or higher with crypto support

###Compiling
* gcc
* [rebar](https://github.com/rebar/rebar)
* git
* erlang-src

####Installing Dependencies on Debian/Ubuntu

    sudo apt-get install rebar erlang-src erlang-xmerl erlang-parsetools

##Download

* Signed [releases](https://people.apache.org/~dch/dist/tools/)
are available from [dch](https://twitter.com/dch__)'s site.
* Unix users will need to `chmod +x erica` and ensure it is on your PATH.
* Windows users should use `erica.cmd` and simply drop it
into your `%COUCHDB%/bin` directory.

##Installation

To install it from source you will have to clone the repository from
github. Dependencies will be fetched during the build.

    $ git clone git://github.com/benoitc/erica.git

You can also download the latested tarball available on the [download
page](https://github.com/benoitc/erica/downloads):

    $ curl -L -O https://github.com/downloads/benoitc/erica/erica-<vsn>.tar.gz

Then build the sources:

    $ cd erica
    $ make

Then you can use the generated script **erica** .

To install it on your system, run the command line:

    $ make install

### Upgrade from  the source repository

When you want to upgrade from the source repository, run the following
commands:

    $ git pull --rebase
    $ make upgrade
    $ make install

## Quick Start: Design Docs

So need to make and mange design docs for couchdb?

    # erica create-app
    # cd myapp
    # erica push myapp

Go ahead and create views, shows, lists, etc.


## Quick Start: Webapps

To create a webapp, follow this pattern.

    # erica create-webapp
    # cd myapp
    # erica push myapp

This is an 'attachment first' style couchapp. Anything related to the design doc lives in _ddoc.

After any changes, push it to your couchdb node

    $ erica push http://127.0.0.1:5984/testdb

Then visit the result on

    http://127.0.0.1:5984/testdb/_design/myapp/_rewrite/

That's it.

Note: By default the CouchDB Node uri is 127.0.0.1:5984 so you could
just use the db name in push command line if you want:eri

    $ erica push testdb


## Detailed Usage:

    # erica command

Where available commands are:

    push           [options...] [dir]    dest  push anything to couchdb
    create-webapp  [appid=myapp] ...     Create a webapp. Default:
                                         appid=myapp, lang=javascript
    create-ddoc    [appid=myapp] ...     Create a blank ddoc, Default:
                                         appid=myapp, lang=javascript
    create-app     appid=AppID lang=Lang Create a blank couchapp, Default:
                                         appid=myapp, lang=javascript
    create         template= [vars...]   create an application using a
                                         template
    init                                 initialize a .couchapprc
    clone          [option] source dir   clone a document from couchdb
    browse                               display the erica in the
                                         browser.
    web            port=Port [dir]       launch the web ui
    help                                 Show the program options
    version                              Show version information


And more general options

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




### 1 . About the Design Doc

You can use a template to create your design doc.
It will create a simple project that you can use for a
start:

    $ erica create-ddoc appid=myapp lang=javascript
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
coffescript, just replace javascript by coffescript or use the language
you want if an couchapp server exists for it.


### 2. Clone

Did you see an interesting couchapp you want to reuse? Or just working
with a friend on the same couchapp ? With the `clone` command you can
replicate a couchapp on your filesystem, edit it and push the results
after:

    $ erica clone http://127.0.0.1:5984/testdb/_design/myapp

This command will clone the couchapp `myapp` in the `myapp` folder. If
you want to clone it to another folder, just do:

    $ erica clone http://127.0.0.1:5984/testdb/_design/myapp mynewapp

## More

Add an `.ericaignore` file to the root of your app, as a JSON array
of regular expressions of files or folders to be excluded from pushes.

    ["passwords.txt", "^\.ssh", "^\.*"]

When using templates, follow this format

    # erica create template=name

Provided templates are for now:

* web (the template used with the create-web command)
* ddoc (the default create used for create-ddoc command)
* example: a simple couchapp exampole
* couchapp: a simple template with the good old couchapp javascript library.

You can add your own template in ~/.erica/templates.


## Getting Help

If you have any questions contact us on irc freenode **#couchapp** or on
the mailing-list: http://groups.google.com/group/couchapp .
