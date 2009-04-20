
var get_components = function() {
    $(".component").each(loadComponent);
};

$(document).ready(get_components);


var loadComponent = function() {
    $(this).css("display", "none");
    $(this).load($(this).attr("evo:url"));
    $(this).css("display", "block");
};


//*******************
// MENUS
//*******************

var current_menu;

var configure_menus = function(sel) {
    $(sel).children("div").each(function() {
        var items = $(this).find("li");
        if (items.length == 1) {
            var link = $(items[0]).find("a");
            $(this).find("h1").empty().append(link);
        } else {
          $(this).hover(function() {
                var menu = $(this).find("ul");
                var position = $(this).position();
                menu.css('left', position.left);
                menu.css('top', position.top + $(this).height());   
                menu.css('width', $(this).css('width'));
                menu.show();
            }, function() {
                $(this).find("ul").hide();
            }
          );
        }
    });
    $(sel).css('display', 'block');
};