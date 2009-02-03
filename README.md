# CouchApp: Standalone CouchDB Application Development Made Simple

CouchApp is a set of helpers and a [jQuery](http://jquery.com) plugin that conspire to get you up and running on [CouchDB](http://couchdb.org) quickly and correctly. It is maintained and designed by CouchDB committers and community-members to bring clarity and order to the freedom of CouchDB's document-based approach.

CouchApp *is by no means the only way to use CouchDB*. CouchDB's technical roots make it well suited for **very large installations**. CouchApp concentrates instead on a more personal use case: **developing and deploying standalone applications to CouchDB instances around the web.** There are apps you can build with server-side components that you can't build with just CouchApp. But by the same token, there are apps you can build on CouchApp alone that you can't build any other way.

## Installation

    sudo gem install couchapp

If this gives you trouble, see the INSTALLING file for more options.

## Begin Here

Once you run `couchapp generate relax && cd relax`, you're ready to get started. Views are found in the `views` directory, attachments are stored in the `_attachments` directory,  forms are stored in `forms`, and the generation script drops some more files in with additional information about how you can build `_design/` docs using your text editor.

## Usage

To upload your application to a CouchDB database, run this command from within you application directory.

    couchapp push . mydb

You can expand `mydb` to be as complex as `http://login:password@my.couchapp.com:5984/mydb` if you need to.

CouchApp provides some neat helpers for getting code into your view and render functions. Look in the view files created by a generated app to see the usage for the `!code` and `!json` preprocessor macros. They are basically just two different ways to get more code into your functions.

### !code

The `!code path.to.code` macro inserts the string value found at `path.to.code` of the design document, into the current file, at the position of the macro. Here's an example:

    function(doc) {
      // !code lib.parser.html
      var parsed = new parseHTML(doc.html);
      emit(doc.key, parsed);
    }

When you run `couchapp push` the `!code` line will be replaced with the contents of the file found at `lib/parser/html.js`. Simple as that.

#### Standard Library

As we begin to see more apps built using CouchApp, we plan to include helpful functions in the standard library (as supplied by `couchapp generate`). Thank you for helping us expand this section! :)

### !json

After all the `!code` includes have been processed (insuring that they may also use the `!json` macro), `couchapp push` does another pass through the function, running the `!json path.to.json` macro. This accumulates the data found at `path.to.json` into a single object for inclusion. After all the `!json` macros have been processed, the accumlated object is serialized to json and stored in a variable with a name corresponding to the path root. 

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

## Apps Using CouchApp

There are a few apps out there already using CouchApp. Please send a pull request adding yours to the list if you're using it too.

* [Sofa](http://github.com/jchris/sofa)
* [Couch-Wiki](http://github.com/janl/couch-wiki)
* [CouchDB Twitter Client](http://github.com/jchris/couchdb-twitter-client)

## License

CouchApp is licensed under the [Apache 2.0 License](http://www.apache.org/licenses/LICENSE-2.0)