# CouchApp: Standalone CouchDB Application Development Made Simple

CouchApp is a set of helpers and a [jQuery](http://jquery.com) plugin that conspire to get you up and running on [CouchDB](http://couchdb.org) quickly and correctly. It is maintained and designed by CouchDB committers and community-members to bring clarity and order to the freedom of CouchDB's document-based approach.

CouchApp *is by no means the only way to use CouchDB*. CouchDB's technical roots make it well suited for **very large installations**. CouchApp concentrates instead on a more personal use case: **developing and deploying standalone applications to CouchDB instances around the web.** There are apps you can build with server-side components that you can't build with just CouchApp. But by the same token, there are apps you can build on CouchApp alone that you can't build any other way.

## Begin Here

Once you run `couchapp generate relax && cd relax`, you're ready to get started. Views are found in the `views` directory, attachments are stored in the `_attachments` directory,  forms are stored in `forms`, and the generation script drops some more files in with additional information about how you can build `_design/` docs using your text editor.

There are a few apps out there already using CouchApp. Please send a pull request adding yours to the list if you're using it too.

## Apps Using CouchApp

* [Sofa](http://github.com/jchris/sofa/tree/master)
* [Couch-Wiki](http://github.com/janl/couch-wiki/tree/master)

## License

CouchApp is licensed under the [Apache 2.0 License](http://www.apache.org/licenses/LICENSE-2.0)