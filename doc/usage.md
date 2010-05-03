#Couchapp Command Line Usage


## Full command line usage

    $ couchapp help
    couchapp [OPTIONS] [CMD] [OPTIONSCMD] [ARGS,...]
    usage:

    -h/--help	 display help and exit
    --version	 display version and exit
    -v/--verbose	 enable additionnal output
    -q/--quiet	 don't print any message

    list of commands:
    -----------------

    clone	 [OPTION]...[-r REV] SOURCE [COUCHAPPDIR]
    -r/--rev [VAL]	 clone specific revision

    generate	 [OPTION]... [app|view,list,show,filter,function,vendor] [COUCHAPPDIR] NAME
    --template [VAL]	 template name

    help	 

    init	 [COUCHAPPDIR]

    push	 [OPTION]... [COUCHAPPDIR] DEST
    --no-atomic	 send attachments one by one
    --export	 don't do push, just export doc to stdout
    --output [VAL]	 if export is selected, output to the file
    -b/--browse	 open the couchapp in the browser
    --force	 force attachments sending
    --docid [VAL]	 set docid

    pushapps	 [OPTION]... SOURCE DEST
    --no-atomic	 send attachments one by one
    --export	 don't do push, just export doc to stdout
    --output [VAL]	 if export is selected, output to the file
    -b/--browse	 open the couchapp in the browser
    --force	 force attachments sending

    pushdocs	 [OPTION]... SOURCE DEST
    --no-atomic	 send attachments one by one
    --export	 don't do push, just export doc to stdout
    --output [VAL]	 if export is selected, output to the file
    -b/--browse	 open the couchapp in the browser
    --force	 force attachments sending

    vendor	 [OPTION]...[-f] install|update [COUCHAPPDIR] SOURCE
    -f/--force	 force install or update
    
    
## Commands

* **generate** : allows you to generate a basic couchapp. It can also be used to create template of functions. Ex:

    couchapp generate myapp
    cd myapp
    couchapp generate view someview

* **init**: Initialize a CouchApp. When run in the folder of your application it create a default `.couchapprc` file. This file is needed by couchapp to find your application. Use this command when you clone your application from an external repository (git, hg):

        cd mycouchapp
        couchapp init
    
* **push**: Push a couchapp to one or more [CouchDB](http://couchdb.apache.org) server

        cd mycouchapp
        couchapp push http://someserver:port/mydb
    
    
    * `--no-atomic` option allows you to send attachments one by one. By default all attachments are sent inline.
    * `--export` options allows you to get the JSON document created. Combined with `--output`, you can save the result in a file.
    * `--force` :  force attachment sending
    * `--docid` option allows you to set a custom docid for this couchapp
    
    
* **pushapps** : like `push` but on a folder containing couchapps. It allows you to send multiple couchapps at once.

        couchapp pushapps somedir/

* **pushdocs**: Like pushapps but for docs. It allows you to send a folder containing simple document. With this command you can populate your CouchDB with documents. Anotther way to do it is to create a `_docs` folder at the top of your couchapp folder.


    
    
    
    
