function(e, r) {
  var userCtx = r.userCtx;
  // todo we need a place to put library functions // application constants
  // maybe we need to make common.js require available in here
  var proid = "couch.app.profile:"+userCtx.name, 
    widget = $(this);

  $$(widget).app.db.openDoc(proid, {
    success : function(doc) {
      // todo decide if this is better than passing around
      $$(widget).profile = doc;
      widget.trigger("profileReady", [doc]);
    },
    error : function() {
      widget.trigger("noProfile", [userCtx]);
    }
  });
}