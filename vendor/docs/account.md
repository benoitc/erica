# Docs for the account widget

You should use this widget anytime you are interested in having users login or signup.

Hopefully you see it is easy to install. To use the account widget, just define a `div` in your page and use [Evently](#/topics/evently) to load it from the design document and apply it to the page.

Here's a basic example:

    $.couch.app(function(app){
      $("#basic").evently(app.ddoc.vendor.couchapp.evently.account);      
    });
