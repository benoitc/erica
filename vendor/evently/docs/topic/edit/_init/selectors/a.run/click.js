function(e) {
  e.preventDefault();
  var id = e.data.args[1];
  var example = $("#code-"+id);
  var js = $('textarea',example).val() || $('pre',example).text();
  $('#'+id).unbind();
  try {
    eval(js);            
  } catch (e) {
    $('#'+id).html('<p>Error running #'+id+' code block:</p><p><pre>'+e.toSource()+'</pre></p>');
  }
  return false;
}