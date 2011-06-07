## Generated CouchApp

This is meant to be an example CouchApp and to ship with most of the CouchApp goodies.

Install with 

    couchapp push . http://localhost:5984/proto

or (if you have security turned on)

    couchapp push . http://myname:mypass@localhost:5984/proto
  
You can also create this app by running

    couchapp generate proto && cd proto
    couchapp push . http://localhost:5984/proto

## Todo

* factor CouchApp Commonjs to jquery.couch.require.js
* use $.couch.app in app.js

## License

Apache 2.0
