function(e) {
  e.preventDefault();
  var form = $(this);
  var doc = {
    created_at : new Date(),
    profile : $$("#profile").profile,
    message : $("[name=message]", form).val()
  };
  var db = $$(this).app.db;

  db.saveDoc(doc, {
    success : function() {
      $("input[name='_rev']", form).val(doc._rev);
      var as = $("input[name='_attachments']", form).val();
      if (as) {
        $("[name=message]", form).val("Uploading file...");
        // thank you cmlenz for Futon's original upload code
        form.ajaxSubmit({
          url: db.uri + $.couch.encodeDocId(doc._id),
          success: function(resp) {
            $("[name=message]", form).val("");
          }
        });
      } else {
        $("[name=message]", form).val("");
      }
    }
  });
  return false;
};
