function (newDoc, oldDoc, userCtx) {
  var doc_type = (oldDoc || newDoc)['doc_type'];
  var author = (oldDoc || newDoc)['author'];
  var docid = (oldDoc || newDoc)['_id'];

  function forbidden(message) {    
    throw({forbidden : message});
  };
  
  function unauthorized(message) {
    throw({unauthorized : message});
  };

  function require(beTrue, message) {
    if (!beTrue) forbidden(message);
  };

}