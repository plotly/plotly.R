// Ping plotly to make sure it's active before posting further messages
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


