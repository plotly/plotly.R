// Immediately Invoked Function Expression (IIFE).
// recommended by Joe -> https://github.com/jcheng5/shiny-js-examples/blob/master/output/www/linechart-binding.js
(function() {

var binding = new Shiny.OutputBinding();

binding.find = function(scope) {
  return $(scope).find('.plotly_shiny');
};

binding.renderValue = function(el, dat) {

    // $el is the jQuery representation of this shiny output
    // for offline plots, this element is a div; otherwise, an iframe
    var $el = $(el);
    
    if (dat[0].offline === true) {
      // why does this add traces instead of create a new plot?
      Plotly.newPlot($el.attr("id"), dat[0].data, dat[0].layout);
    } else {
      // see https://github.com/plotly/postMessage-API#newPlot
      var msg = {task: 'newPlot', data: dat[0].data, layout: dat[0].layout};
      var plot = $el[0].contentWindow;
      // If we haven't attached any data to this binding, ping plotly to 
      // make sure it's ready to receive message(s). This will prevent posting
      // n >= 1 messages everytime shiny inputs change
      // Combines ideas from:
      // https://github.com/plotly/postMessage-API#ping
      // https://github.com/jcheng5/shiny-js-examples/blob/master/output/www/linechart-binding.js
      if (!$el.data("plotly")) {
        var pinger = setInterval(function() {
          plot.postMessage({task: 'ping'}, 'https://plot.ly');
        }, 500);
        window.addEventListener('message', function(e) {
          if (e.origin !== "https://plot.ly") return;
          if (e.data.pong) {
            console.log('Initial pong, frame is ready to receive');
            clearInterval(pinger);
            plot.postMessage(msg, 'https://plot.ly');
          }
        });
        // now attach some arbitrary data
        $el.data("plotly", {plotly: 'rules'});
      }
      
      plot.postMessage(msg, 'https://plot.ly');
    }
    
};

Shiny.outputBindings.register(binding, "plotlyEmbed");

})();
