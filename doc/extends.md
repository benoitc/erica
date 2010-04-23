#Extend couchapp

Couchapp can easily be extended using external python modules or scripts. There are 3 kind of extensions:

- extensions: allows you to add custom commands to couchapp
- hooks: allows you to add actions on pre-/post (push, clone, pushdocs, pushapps) events.
- vendors handlers: allows you to add support for different sources of vendor

## Extensions:

Extensions are eggs or python modules registered in the config file in "extensions" member, ex:

    "extensions": [
       "egg:mymodule#name"
    ]

Eggs uri are entry points uri starting with "egg:" prefix. To just load python module use an uri with the form : "python:mymodule.myextension".

To load eggs add an entry point in "couchapp.extension" sections. More info about entry points [here](http://packages.python.org/distribute/pkg_resources.html#entry-points)

An extension is a python module loaded when couchapp start. You can add custom commands to couchapp via this way. To add custom commands simply add a dict named **"cmdtable"**. This dict is under the format :

    cmdtable = {
        "cmdname": 
            (function, 
            params, 
            "help string")
    }

`Params` is a list of options that can be used for this function (the -someoption/--someoption= args) :

    params = [
     ('short', 'long', default, "help string"),
    ]

`short` is the short option used on command line (ex: -v)
`long` is the long option (ex: --verbose)

`default` could be True/False/None/String/Integer

## Hooks

Couchapp offers a powerful mechanism to let you perform automated actions in response of different couchapp events (push, clone, generate, vendor).

Hooks are eggs or python modules registered in the config file in "hooks" member, ex:

    "hooks": {
        "pre-push": [
            "egg:couchapp#compress"
        ]
    }

Like extennsions egg uri start with "egg:" prefix and python module with "python:". Entry point are added to **"couchapp.hook"** distribution. Here is the declaration of coupress hook in couchapp setup.py :

    setup(
        name = 'Couchapp',
        ...
        
        entry_points="""
        ...
        
        [couchapp.hook]
        compress=couchapp.hooks.compress:hook

        ...
        """,

        ...
    )
    
hooks are python functions like this:

    def hook(path, hooktype, **kwarg):
        ...
        
path is the directory of the CouchApp on the filesystem, hooktype is the name of the event `pre-/post-(push|clone|generate|vendor)` and kwargs a list of arguments depending on the event:

- push: `dbs`: list of Database object
- clone: `source`: the source of the couchapp to clone
- vendor:  `source`, the uri of vendor,  `action`, could be *install* or *update*.
- generate: None

Have a look in [compress hook source](http://github.com/couchapp/couchapp/tree/master/couchapp/hooks/compress/) for a complete example.

## Vendors handlers

for vendor_uri in self.conf.get('vendors'):
    obj = util.parse_uri(vendor_uri, "couchapp.vendor")
    vendors_list.append(obj)

Vendors handlers are used to manage installation or update of vendors. Like extensions or hooks vendors handlers are eggs or python modules registered in config file:

    {
        "vendors": [
            "egg:couchapp#git",
            "egg:couchapp#hg",
            "egg:couchapp#couchdb"
        ]
    }

(above is the default). Entry point are added to **"couchapp.vendor"** distribution, ex:

    setup(
        name = 'Couchapp',
        ...
        
        entry_points=""" 
        [couchapp.vendor]
        git=couchapp.vendors.backends.git:GitVendor
        hg=couchapp.vendors.backends.hg:HgVendor
        couchdb=couchapp.vendors.backends.couchdb:CouchdbVendor

        ...
        """,

        ...
    )

A vendor is an object inheriting `couchapp.vendor.base.BackendVendor` class:

    class MyVendor(BackendVendor):
        """ vendor backend interface """
        url = "",
        license =  "",
        author = "",
        author_email = "",
        description = ""
        long_description = ""

        scheme = None

        def fetch(url, path, *args, **opts):
            raise NotImplementedError
            
            
- url: is the url of the vendor source
- license: the license of the vendor
- author: name of author
- author_email: email of author
- description: short description of this vendor
- long_descrtiption: long description
- scheme: list of url prefix on which this handler will be use. (ex: ['git', 'git+ssh'] for git://|git/ssh:// urls)


the `fetch` function take the url given in console, the path of couchapp.
    
Here is an example for the default git vendor:

    class GitVendor(BackendVendor):
        url="http://github.com/couchapp/couchapp"
        author="Benoit Chesneau"
        author_email="benoitc@e-engura.org"
        description = "Git vendor handler"
        long_description = """couchapp vendor install|update from git::
    
        git://somerepo.git (use git+ssh:// for ssh repos)
        """
    
        scheme = ['git', 'git+ssh']

        def fetch(self, url, path, *args, **opts):
            ....
            
Full source is [on the git repo](http://github.com/couchapp/couchapp/blob/master/couchapp/vendors/backends/git.py).



