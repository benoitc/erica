CouchApp: Standalone CouchDB Application Development Made Simple

CouchApp is designed to structure standalone CouchDB application
development for maximum application portability.

CouchApp is a set of scripts and a jQuery plugin designed to bring
clarity and order to the freedom of CouchDB's document-based approach.
Write apps using just JavaScript and HTML

Render HTML documents using JavaScript templates run by CouchDB. You'll
get parallelism and cacheability, using only HTML and JS. Building
standalone CouchDB applications according to correct principles affords
you options not found on other platforms.
Deploy your apps to the client

CouchDB's replication means that programs running locally, can still be
social. Applications control replication data-flows, so publishing
messages and subscribing to other people is easy. Your users will see
the benefits of the web without the hassle of requiring always-on
connectivity.


##Installation

Couchapp requires Erlang R13B04 or sup.

    $ git clone git://github.com/benoitc/erlca.git
    $ cd erlca
    $ make

Then you can use the generated script **couchapp** .
