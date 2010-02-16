function() {
  // TODO this can be cleaned up with docForm
  // todo get the app from somewhere
  var name = $("input[name=userCtxName]",this).val();
  var proid = "couch.app.profile:"+name, 
  newProfile = {
    _id : proid,
    type : "couch.app.profile",
    rand : Math.random().toString(),
    name : name, 
    nickname : $("input[name=nickname]",this).val(),
    email : $("input[name=email]",this).val(),
    url : $("input[name=url]",this).val()
  }, widget = $(this);
  var app = $$(widget).app;

  // setup gravatar_url
  if (typeof hex_md5 == "undefined") {
    alert("creating a profile requires md5.js to be loaded in the page");
    return;
  }

  newProfile.gravatar_url = 'http://www.gravatar.com/avatar/'+hex_md5(newProfile.email || newProfile.rand)+'.jpg?s=40&d=identicon';

  app.db.saveDoc(newProfile, {
    success : function() {
      app.db.openDoc(proid, {
        success : function(doc) {
          widget.trigger("profileReady", [doc]);
        }
      });
    }
  });
  return false;
}