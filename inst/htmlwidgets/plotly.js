HTMLWidgets.widget({
  name: "plotly",
  type: "output",
  
  initialize: function(el, width, height){
    return {};
  },
  
  // NOTE: when you view a normal ggplot2 plot, and resize your graphics device,
  // the graph 'domains' change, but everything else is fixed (I think).
  // This only matters when we have more than one panel, free scales,
  // and/or interior facet strips, since our only option is add space in axis
  // domains (measured on a 0-1 scale)
  resize: function(el, width, height, instance) {
    /*
    var nPlots = 2, nRows = 1, margins = [0, 0, 0, 0];
    var nCols = Math.ceil(nPlots / nRows);
    var lPad = 0, rPad = 0;
    var doms = [];
    for (i = 1; i <= nCols; i++) { 
      if (i !== 1) { 
        lPad = margins[0]; 
      }
      if (i !== nCols) {
        rPad = margins[1];
      }
      var dom = [((i - 1) / nCols) - lPad, (i / nCols) + rPad].sort();
      doms.push(dom);
    }
    */
    el2 = el;
    Plotly.relayout(el.id, {width: width, height: height});
  },  
  
  renderValue: function(el, x, instance) {
    // make sure plots don't get created outside the network
    window.PLOTLYENV = window.PLOTLYENV || {};
    window.PLOTLYENV.BASE_URL = x.base_url;
    
    el.gglayout = x.layout.gglayout;
    
    // if no plot exists yet, create one with a particular configuration
    if (!instance.plotly) {
      Plotly.plot(el.id, x.data, x.layout, x.config);
      instance.plotly = true;
    } else {
      Plotly.newPlot(el.id, x.data, x.layout);
    }
  }
  
});
