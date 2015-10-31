HTMLWidgets.widget({
  name: "plotly",
  type: "output",
  
  initialize: function(el, width, height){
    return {};
  },
  
  resize: function(el, width, height, instance) {
    Plotly.relayout(el.id, {width: width, height: height});
  },  
  
  renderValue: function(el, x, instance) {
    // if no plot exists yet, create one with a particular configuration
    if (!instance.plotly) {
      Plotly.plot(el.id, x.data, x.layout, x.config);
      instance.plotly = true;
    } else {
      Plotly.newPlot(el.id, x.data, x.layout);
    }
  }
  
});
