# Evently Docs

Evently is an evented framework for jQuery. You write your application as widgets made up of templates and callbacks, while Evently handles the busywork of linking them together.

Evently has special handlers for CouchDB views and `_changes` feeds, and could be easily extended for other server-side frameworks.

## Hello World

At it's simplest an Evently widget is a set of events connected to a single DOM element.

JavaScript:

    $("#hello").evently({
      _init : {
        mustache : "<p>Hello world</p>",
      },
      click : {
        mustache : "<p>What a crazy world!</p>",        
      }
    });

You can also do some more interesting things:

    $("#heyjane").evently({
      _init : {
        mustache : '<p>Hello <a href="#jane">Jane</a>, <a href="#joan">Joan</a> (pick one)</p>',
        selectors : {
          'a[href=#joan]' : {
            click : 'hiJoan'
          },
          'a[href=#jane]' : {
            click : 'hiJane'
          }
        }
      },
      hiJoan : {
        mustache : '<p>Hello Joan!</p>'
      },
      hiJane : {
        mustache : "<p>Darn, it's Jane...</p>",
        after : function() {
          setTimeout(function() {
            // automatically trigger the "janeRocks" event after 2 seconds.
            $("#heyjane").trigger("janeRocks");
          }, 2000);
        }
      },
      janeRocks : {
        render : "append",
        mustache : "<p>Actually Jane is awesome.</p>"
      }
    });


The imporant thing about this is that the widget is defined by an JavaScript object. This means we can save it as files on our hard drive and `couchapp` will handle saving it as a JSON object for us.

[screenshot of the above code in textmate's file drawer]

When we let CouchApp package our evently apps we get to work on them in individual files, instead of as a great big giant mess of JavaScript. This means HTML is HTML, JSON is JSON, and JavaScript is JavaScript. Yay!

## Ajax Hello World

Let's do a little Ajax. We'll just load the version of the CouchDB instance we happen to be serving our HTML from:

    $("#ajax").evently({
      _init : {
        mustache : '<p>Loading CouchDB server info.</p>',
        after : function() {
          var widget = $(this);
          $.ajax({
            url : '/',
            complete : function(req) {
              var resp = $.httpData(req, "json");
              widget.trigger("version", [resp]);
            }
          })
        }
      },
      version : {
        mustache : "<p>Running CouchDB version {{version}}</p>",
        data : function(e, resp) {
          return resp;
        }
      }
    });

-- triggering other events
  -- selectors
  -- create a doc

-- storing the code in couchapp file tree

-- run a query

-- connect to changes

-- links to example apps

pathbinder

A tour of Taskr