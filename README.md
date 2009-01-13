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

## Apps Using CouchApp

There are a few apps out there already using CouchApp. Please send a pull request adding yours to the list if you're using it too.

* [Sofa](http://github.com/jchris/sofa)
* [Couch-Wiki](http://github.com/janl/couch-wiki)
* [CouchDB Twitter Client](http://github.com/jchris/couchdb-twitter-client)

## License

CouchApp is licensed under the [Apache 2.0 License](http://www.apache.org/licenses/LICENSE-2.0)