
var get_components = function() {
    $(".component").each(function() {
        console.debug("Component URL: " + $(this).attr("evo:url"));
        console.debug("Component tag: " + $(this).html());
        $(this).load($(this).attr("evo:url"));
    });

}

$(document).ready(get_components);