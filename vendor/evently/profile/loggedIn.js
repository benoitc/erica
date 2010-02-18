function(e, r) {
  var userCtx = r.userCtx;
  var widget = $(this);
  // load the profile from the user doc
  $.couch.userDb(function(db) {
    var userDocId = "org.couchdb.user:"+userCtx.name;
    db.openDoc(userDocId, {
      success : function(userDoc) {
        var profile = userDoc["couch.app.profile"];
        if (profile) {
          $$(widget).profile = profile;
          widget.trigger("profileReady", [profile]);
        } else {
          widget.trigger("noProfile", [userCtx]);
        }
      }
    });
  });
}
