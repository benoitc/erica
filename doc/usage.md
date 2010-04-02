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