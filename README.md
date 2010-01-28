# CouchApp: Standalone CouchDB Application Development Made Simple

CouchApp is designed to structure standalone CouchDB application development for maximum application portability.

CouchApp is a set of scripts and a [jQuery](http://jquery.com) plugin designed  to bring clarity and order to the freedom of [CouchDB](http://couchdb.org)'s document-based approach.

### Write apps using just JavaScript and HTML

Render HTML documents using JavaScript templates run by CouchDB. You'll get parallelism and cacheability, **using only HTML and JS.** Building standalone CouchDB applications according to correct principles affords you options not found on other platforms.

### Deploy your apps to the client

CouchDB's replication means that programs running locally, can still be social. Applications control replication data-flows, so publishing messages and subscribing to other people is easy. Your users will see the benefits of the web without the hassle of requiring always-on connectivity.

## Installation

Couchapp requires Python 2.5x or sup. To install couchapp using easy_install you must make sure you have a recent version of setuptools installed (as of this writing, 0.6c6 (0.6a9 on windows) or later):

    $ curl -O http://peak.telecommunity.com/dist/ez_setup.py
    $ sudo python ez_setup.py -U setuptools

To install or upgrade to the latest released version of couchapp:

    $ sudo easy_install -U couchapp

If this gives you trouble, see the INSTALLING file for more options.

### Begin Here

Once you run `couchapp generate relax && cd relax`, you're ready to get started. Views are found in the `views` directory, attachments are stored in the `_attachments` directory,  forms functions are stored in `list` and `show`, and the generation script drops in additional explantory files.


## There's more to Couch than this

CouchApp *is by no means the only way to use CouchDB*. CouchDB's technical roots make it well suited for **very large installations**.  CouchDB excels at document management, archived stream processing (like logs or messaging), and other applications with voluminous data.

CouchApp concentrates instead on a more personal use case: **developing and deploying standalone applications to CouchDB instances around the web.**

### It's the portability

There are apps you can build with server-side components that you can't build with just CouchApp. But by the same token, there are apps you can build on CouchApp alone that you can't build any other way. The flexibility of replication means that there are yet-undiscovered ways to mix datacenter level clusters with end-user installations.

# Usage

To upload your application to a CouchDB database, run this command from within you application directory. In this example we assume you have a copy of CouchDB running on your local machine.

## Push

    couchapp push http://localhost:5984/mydb

You can use this URL-form to send credentials data if you need to: 

`http://login:password@my.couchapp.com:5984/myapp` 

At the bottom of this readme there is information about the `.couchapprc` file. You want to read this and use it. It will make you happy because you won't need to specify the database URL to push.

### Push Helper Macros

CouchApp provides some neat helpers for getting code into your view and render functions. Look in the view files created by a generated app to see the usage for the `!code` and `!json` preprocessor macros. They are basically just two different ways to get more code into your functions.

### !code for code

The `!code path/to/code.js` macro inserts the content of the named file, into the current file, at the position of the macro. Here's an example:

    function(doc) {
      // !code lib/parsers/parse_html.js
      var parsed = new parseHTML(doc.html);
      emit(doc.key, parsed);
    }

When you run `couchapp push` the `!code` line will be replaced with the contents of the file found at `lib/parsers/parse_html.js`. Simple as that.

As we begin to see more apps built using CouchApp, we plan to include helpful functions in the standard library (as supplied by `couchapp generate`). Thank you for helping us expand this section! :)

### !json for data

After all the `!code` includes have been processed (insuring that included code may also use the `!json` macro), `couchapp push` does another pass through the function, running the `!json path.to.json` macros. This accumulates the data found in the JSON design doc at `path.to.json` into a single object for inclusion. After all the `!json` macros have been processed, the accumlated object is serialized to json and stored in variable names corresponding to their path's roots. 

The JSON paths use JavaScript style dot notation. Address the nodes as though you are running inside a `with(designDoc){ ... }` block. See the examples below.

Here's an example. It's a lot of code to look at, but the principles are simple. Also, if you think you can explain it better than I have, please send a patch.

#### A Subset of the Design Doc Fields (for context)

    {
      "lib" : {
        "templates" : {
          "post" : "<html> ... </html>",
          "comment" : "<html> ... </html>"
        },
        "render" : {
          "html" : "function(template, object){...}"
        }
      },
      "blog" : {
        "title" : "My Rad Blog"
      }
    }

#### The Function

    function(doc) {
      // !json lib.templates.post
      // !json blog.title  
      ...
      doSomething(lib.templates.post, blog.title);
    }

#### The Result

    function(doc) {
      var lib = {
        "templates" : {
          "post" : "<html> ... </html>"
        }
      };
      var blog = {
        "title" : "My Rad Blog"
      };
      ...
      doSomething(lib.templates.post, blog.title);
    }

The upshot is that only the requested fields are included in the function. This allows you to put as many templates and libraries in your design doc as you wish, without creating overlong functions.

#### Silly Counter-example

    function(doc) {
      // !json lib
      // !json lib.templates.post
    }

In this example, the second usage of the macro is redundant, as the first usage will include the entire `lib` field as a JSON macro.

#### Sharing Code From Attachments into Views, Lists, and Shows

!json and !code also work for the `_attachments/` directory.

    // !json _attachments/file.ext 

will create the variable _attachments['file.ext'].

    // !code _attachments/file.ext 

will include the contents of the file.

For anything but `_attachments`, the include should use ".". Duplicate property names (two files with the same name but different extensions) will print an error in verbose level = 2.

### Deployment preferences in `.couchapprc`

You can set up application level helpers in the `.couchapprc` file

The format is like this:

    {
      "env": { 
        "default": {
          "db": "http://user:pass@localhost:5984/myapp-dev"
        },
        "production": {
          "db": "http:///user:pass@example.com/myapp"
        }
      }
    }

When you've set up `.couchapprc` you can push your app with just `couchapp push` or for non-default environments `couchapp push production`. This also has the advantage of not requiring password use on the command line. The `.couchapprc` file is not pushed with the rest of the design doc, but please be careful not to accidentally check your `.couchapprc` file into Git!

## Clone

Clone downloads apps from other databases around the internet, all you have to do is point to the design doc url. Usage instructions will be here soon.

## Vendor

Handle vendor update and install from a git repository.

### How Vendor Apps Should Be Configured With Your Repository

Within your repository, your vendor app should be in a vendor folder:

    vendor/appname

For example, the javascript that is vendored (by default) with your use of couchapp is in the repo git://github.com/couchapp/couchapp.git, located in the path:

    vendor/couchapp

### Commands

To update a vendor folder in your couchapp:

    couchapp vendor update <app dir>

Or from within the app dir:
    
    couchapp vendor update

To install a vendor app:

    couchapp vendor install git://somerepo [app dir]

Example:

    couchapp vendor install git://github.com/jchris/couchapp.git

CouchApp's JavaScript library is a vendor (the one right above this line) and it is installed by default in new generators.

# Community

There is a mailing list here: [http://groups.google.com/group/couchapp](http://groups.google.com/group/couchapp)

Also, join us on [irc.freenode.net in the #couchapp room](irc://irc.freenode.net/couchapp).

### Apps Using CouchApp

There are a [few apps](http://wiki.github.com/couchapp/couchapp/apps) out there already using CouchApp. Please send a pull request adding yours to the list if you're using it too.

* [Sofa](http://github.com/jchris/sofa)
* [CouchDB Twitter Client](http://github.com/jchris/couchdb-twitter-client)


## License

CouchApp is licensed under the [Apache 2.0 License](http://www.apache.org/licenses/LICENSE-2.0)
