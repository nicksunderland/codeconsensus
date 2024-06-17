$( document ).ready(function() {

  Shiny.addCustomMessageHandler('cascadeNodes', function(message) {

    var tree = $.jstree.reference(message.id);
    if (tree) {
      if (message.cascade) {
        tree.settings.checkbox.three_state = true;
        tree.settings.checkbox.cascade = 'undetermined+up+down';
      } else {
        tree.settings.checkbox.three_state = false;
        tree.settings.checkbox.cascade = 'undetermined';
      }
    }

/*    console.log(tree)*/

  });


  Shiny.addCustomMessageHandler('expandNodes', function(message) {

    var tree = $.jstree.reference(message.id);
    if (tree) {
      if (message.expand) {
        tree.open_all();
      } else {
        tree.close_all();
      }
    }

  });


  Shiny.addCustomMessageHandler('hideNodes', function(message) {

    var tree = $.jstree.reference(message.id);
    var json = tree.get_json(null, {flat: true});
    for(var i = 0; i < json.length; i++) {
      var id = json[i].id;
      var count = parseFloat(json[i].data[message.code_display]);
      var count_filter = parseFloat(message.count_filter);
      if(count !== null && !isNaN(count) && count < count_filter) {
        tree.hide_node(id);
      } else {
        tree.show_node(id);
      }
    }
  });


/*  Shiny.addCustomMessageHandler('updateFilter', function(message) {

    var slider = $('#' + message.id).data('ionRangeSlider');
    console.log(slider)

    slider.update({
      max: message.max,
      value: 1,
      grid: true,
      grid_num: 5,
      prettify: function (num) {
        return (Math.round(num / 100) * 100);
      }
    })

  });*/


/*  Shiny.addCustomMessageHandler('updateSaveButton', function(message) {

    var btn = document.getElementById(message.id);
    console.log(btn)
    btn.style.color = message.color;

  });
*/





});

