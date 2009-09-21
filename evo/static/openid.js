
var openid_setup = function() {
    $("#openid_identifier").removeAttr("disabled");
    $("#openid_button").removeAttr("disabled");
};

$(document).ready(openid_setup);

var openid_prepare = function() {
    $("#openid_identifier").attr("disabled", "disabled");
    $("#openid_button").attr("disabled", "disabled");
    $("#openidStatus").html("Thinking ...");

    var identifier = $("#openid_identifier").val()

    $.post("/session/openid/prepare", {"openid_identifier": identifier},
           openid_redirect, "json");
};

var openid_redirect = function(data, textStatus) {
    if (data['success']) {
        $("#openidStatus").html("Redirecting ...");
        document.location.href = data['url'];
    } else {
        $("#openidStatus").html("Discovery failed");
        $("#openid_identifier").removeAttr("disabled");
        $("#openid_button").removeAttr("disabled");
    }
};