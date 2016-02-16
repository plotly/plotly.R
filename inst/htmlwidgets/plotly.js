HTMLWidgets.widget({
  name: "plotly",
  type: "output",
  
  initialize: function(el, width, height){
    return {
      width: width, 
      height: height, 
      plot: false
    };
  },
  
   /*
    When you view a normal ggplot2 plot, and resize your graphics device,
    the graph 'domains' change, but everything else is fixed.
    This only matters when we have more than one panel, free scales,
    and/or facet strips, since our only option is add space in axis
    domains (measured on a 0-1 scale)
   */
  
  resize: function(el, width, height, instance) {
    instance.layout.width = width;
    instance.layout.height = height;
    this.renderValue(el, undefined, instance);
  },  
  
  renderValue: function(el, x, instance) {
    
    var graphDiv = document.getElementById(el.id);
    
    // function to reduce `layout.axisid.domain` by `layout.axisid.margins` 
    // (if specified)
    var alterDomain = function(layout) {
      // pixels -> 0-1 scale
      var xNorm = function(x) { return x / instance.width };
      var yNorm = function(x) { return x / instance.height };
      var layoutKeys = Object.keys(layout);
      for (i = 0; i <= layoutKeys.length - 1; i++) {
        // adjust x domain
        if (layoutKeys[i].match(/^xaxis/)) {
          var x = layout[layoutKeys[i]];
          if (x !== undefined && x.hasOwnProperty("margins")) {
            var xMargins = x.margins.map(xNorm);
            layout[layoutKeys[i]].domain = [
              x.domain[0] + xMargins[0], 
              x.domain[1] - xMargins[1]
            ];
          }
        }
        // adjust y domain
        if (layoutKeys[i].match(/^yaxis/)) {
          var y = layout[layoutKeys[i]];
          if (y !== undefined && y.hasOwnProperty("margins")) {
            var yMargins = y.margins.map(yNorm);
            layout[layoutKeys[i]].domain = [
              y.domain[0] + yMargins[0], 
              y.domain[1] - yMargins[1]
            ];
          }
        }
        
      }
      return layout;
    };
    
    // is this the first call to renderValue?
    if (!instance.plot) {
      instance.plot = true;
      // attach layout to the instance so we can access it during a resize event
      instance.layout = x.layout;
      // TODO: get rid of me when you figure out Plotly.relayout()
      instance.data = x.data;
      // make sure plots don't get created outside the network
      window.PLOTLYENV = window.PLOTLYENV || {};
      window.PLOTLYENV.BASE_URL = x.base_url;
      Plotly.plot(graphDiv, x.data, alterDomain(x.layout), x.config);
      console.log("plot");
      
    } else if (x === undefined) {
      // TODO: use Plotly.relayout() instead of Plotly.newPlot()
      // For some reason, even this was throwing an TypeError:
      // Plotly.relayout(graphDiv, {title: "hi there" })
      // Uncaught TypeError: Cannot read property 'selectAll' of undefined(…)
      //var lay = instance.layout;
      console.log(instance.layout);
      Plotly.newPlot(graphDiv, instance.data, alterDomain(instance.layout));
      //instance.layout = lay;
      
    } else {
      Plotly.newPlot(graphDiv, x.data, x.layout);
    }
    
  }
  
});
