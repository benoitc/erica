function(data) {
  var db = $$(this).app.db;
  function attachments(doc) {
    var as = [], a = doc._attachments;
    if (a) {
      for (var n in a) {
        as.push({name: n, path: ["",db.name,doc._id,n].join('/')});
      }
      return as;
    }
  };
  var p;
  return {
    items : data.rows.map(function(r) {
      p = r.value.profile;
      p.message = r.value.message;
      p.atts = attachments(r.value);
      return p;
    })
  }
};