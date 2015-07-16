// First, ping plotly to make sure it's active before posting further messages
// see https://github.com/plotly/postMessage-API#ping

// Before pinging, wait until the entire page is ready
// since iframe(s) might not be ready yet
$(window).load(function() {
  var pinger = setInterval(function() {
    // This class has to match the class in plotlyOutput() as well as
    // Shiny.binding.find (see inst/shiny/plotly_shiny.js)
    var plot = document.getElementsByClassName('plotly_shiny')[0].contentWindow;
    plot.postMessage({task: 'ping'}, 'https://plot.ly');
  }, 500);

  window.addEventListener('message', function(e) {
    if (e.origin !== "https://plot.ly") return;
    if (e.data.pong) {
      console.log('Initial pong, frame is ready to receive');
      clearInterval(pinger);
    }
  });
});



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
      $el[0].contentWindow.postMessage({
           task: 'newPlot',
           data: dat[0].data,
           layout: dat[0].layout
      }, 'https://plot.ly');
    }
    
};

Shiny.outputBindings.register(binding, "plotlyEmbed");

})();
