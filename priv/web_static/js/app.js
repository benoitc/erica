function notify(css, message) {
    var alertDiv = $('<div></div>')
          .html('<a href="#" class="close">x</a>'
            + '<p>' + message + '</p>')
          .addClass('alert-message ' + css + ' fade in')
          .attr("data-alert", "alert");
    $("#notifyBox").append(alertDiv);
    
    window.setTimeout(function() {
      alertDiv.removeClass("in");
      alertDiv.fadeOut("slow", function() {
        alertDiv.remove();
      });
    }, 2000);
}


function create(files, win, callback) {
    if (!files) return;

    $.ajax({
        url: "/create",
        type: "POST",
        dataType: "json",
        contentType: "application/json",
        data: JSON.stringify({"actions": files}),
        success: function(data) {
            $(win).modal('hide');
            if (callback) {
                callback();
            } else {
                notify("success", "<strong>Ok!</strong>");
            }
        },
        error: function(xhr) {
            var reason = xhr.statusText;
            notify("error", '<strong>Error!</strong> ' + reason);
            $(win).modal('hide');
        }
    });
}

function file_action(filename, content) {
    return {"file": {
                 "filename": filename,
                 "content": content}};
}

function fix_evently(p) {
    return p.replace("#", "%23");
}

function relpath() {
    var p = $("#relpath").val();
    if (p) {
        return p + "/";
    } else {
        return p;
    }
}

$(document).ready(function() {

    var AOpts = {
            "placement": "left",
            "offset": 10};

    $("#bmkdir").popover(AOpts);
    $("#badd").popover(AOpts);
    $("#bupload").popover(AOpts);
    $("#bview").popover(AOpts);

   
    $(".cancel").click(function(e) {
        e.preventDefault();
        $(this).parent().parent().parent().modal('hide');
    });


    $("#fmkdir").submit(function(e) {
        e.preventDefault();

        var dirname = $("#dirname").val();
        if (!dirname) {
            alert("dir name is empty");
            return false;
        }
        
        var path = relpath() + dirname;
        var actions = [
            {"dir": { "dirname": path }}
        ];
        create(actions, "#wmkfir", function() {
            window.location.reload();
        });
        return false;
    });

    $("#fadd").submit(function(e) {
        e.preventDefault();

        var filename = $("#filename").val();
        if (!filename) {
            alert("file name is empty");
            return false;
        }

        var path = relpath() + filename;

        var actions = [
            file_action(path, "")
        ];
        create(actions, "#wadd", function() {
            window.location.href="/edit/" + fix_evently(path);
        });
        return false;
    });


    $("#fview").submit(function(e) {
        e.preventDefault();
        
        var vname = $("#vname").val();
        if (!vname) {
            alert("view name is empty");
            return false;
        }

        var vpath = "views/" + vname;

        var actions = [
            file_action(vpath + "/map.js", 
                "function(doc) {\n" 
                + " emit(doc._id, null);\n"
                + "}")
        ];

        if ($("#reduce").is(":checked")) {
            actions.push( 
                file_action(vpath + "/reduce.js", 
                    "function(keys, values, rereduce) {\n\n}")
            );
        }

        create(actions, "#wview", function() {
            window.location.href="/tree/views";
        });
        return false;
    });
    
    $("#fupload").submit(function(e) {
        e.preventDefault();
        $("#fupload").ajaxSubmit({
            dataType: 'json',
            success: function(data) {
                $("#wupload").modal('hide');

                notify("success", "<strong>Ok!</strong>");
                window.location.reload();

            },
            error: function(xhr) {
                var reason = xhr.statusText;
                notify("error", '<strong>Error!</strong> ' + reason);
                $("#wupload").modal('hide');
            }
        });
        return false;
    });
            

    /* ---------------------------
     * push feature
     * --------------------------- */

    $("a[rel=twipsy]").twipsy({
        live: true              
    });

    $("#fpush").submit(function(e) {
        e.preventDefault();
        return false;
    }); 

    $("#cpush").click(function(e) {
        e.preventDefault();
        $("#wpush").modal('hide');
    });
   
    $("#fpush").submit(function(e) {
        e.preventDefault();

        var pushUrl = $("#push_url").val();
        if (!pushUrl) {
           alert("url is empty");
           return false;
        } 
        $.ajax({
            url: "/push",
            type: "POST",
            dataType: "json",
            contentType: "application/json",
            data: JSON.stringify({"url": pushUrl}),
            success: function(data) {
                notify("success", "<strong>Application pushed</strong> on " 
                    + data.url);
                $("#wpush").modal('hide');
            },
            error: function(xhr) {
                var reason = xhr.statusText;
                notify("error", '<strong>Error!</strong> ' + reason);
                $("#wpush").modal('hide');
            }
        });


        return false;
    });

    
});

