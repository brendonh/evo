
var get_components = function() {
    $(".component").each(loadComponent);
};

$(document).ready(get_components);


var loadComponent = function() {
    $(this).css("display", "none");
    $(this).load($(this).attr("evo:url"));
    $(this).css("display", "block");
};

var configure_menus = function(sel) {
    $(sel).children("div").each(function() {
        //$items = $(this).child
    });
};

var whatever = function() {
    $("#siteNavigation img").click(function() {
        var catName = $(this).attr('id').split('_', 2)[1];
        var menuID = '#navigationMenu_' + catName;
        var menu = $(menuID);

        if (!menu.length) {
            if (current_menu) current_menu.hide();
            current_menu = undefined;
            return;
        }

        if (menu.is(':hidden')) {

            if (current_menu) current_menu.hide();
            current_menu = undefined;

            var position = $(this).position();
            menu.css('left', position.left);
            menu.css('top', position.top + $(this).height() - 1);        
            menu.show();

            current_menu = menu;
        } else {
            menu.hide();
        }

    });

};