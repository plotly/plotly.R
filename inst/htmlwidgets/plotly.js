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
    the graph 'domains' change, but everything else is fixed (I think).
    This only matters when we have more than one panel, free scales,
    and/or interior facet strips, since our only option is add space in axis
    domains (measured on a 0-1 scale)
   */
  
  resize: function(el, width, height, instance) {
    instance.width = width;
    instance.height = height;
    this.renderValue(el, undefined, instance);
  },  
  
  renderValue: function(el, x, instance) {
    
    var graphDiv = document.getElementById(el.id);
    
    var alterDomain = function(layout) {
      var xNorm = function(x) { return x / instance.width };
      var xAxes = layout.xaxes || [];
      for (i = 0; i <= xAxes.length - 1; i++) {
        var x = layout[xAxes[i]];
        // translate pixels to 0-1 scale
        x.margins = x.margins.map(xNorm);
        layout[xAxes[i]].domain = [
          x.domain[0] + x.margins[0], 
          x.domain[1] - x.margins[1]
        ];
      }
      var yNorm = function(x) { return x / instance.height };
      var yAxes = layout.yaxes || [];
      for (i = 0; i <= yAxes.length - 1; i++) {
        var y = layout[yAxes[i]];
        y.margins = y.margins.map(yNorm);
        layout[yAxes[i]].domain = [
          y.domain[0] + y.margins[0], 
          y.domain[1] - y.margins[1]
        ];
      }
      return layout;
    };
    
    // is this the first call to renderValue?
    if (!instance.plot) {
      instance.plot = true;
      // attach layout to the instance so we can access it during a resize event
      instance.layout = x.layout;
      // make sure plots don't get created outside the network
      window.PLOTLYENV = window.PLOTLYENV || {};
      window.PLOTLYENV.BASE_URL = x.base_url;
      Plotly.plot(graphDiv, x.data, alterDomain(x.layout), x.config);
      
    // resize event
    } else if (x === undefined) {
      // TODO: why are the annotations being removed?
      console.log(instance.layout);
      var lay = alterDomain(instance.layout);
      console.log(lay);
      Plotly.relayout(graphDiv, lay);
    // render a new plot if in, e.g., a shiny context
    } else {
      Plotly.newPlot(graphDiv, x.data, x.layout);
    }
  }
  
  
  
});
