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
    // make sure plots don't get created outside the network
    window.PLOTLYENV = window.PLOTLYENV || {};
    window.PLOTLYENV.BASE_URL = x.base_url;
    
    // if no plot exists yet, create one with a particular configuration
    if (!instance.plotly) {
      Plotly.plot(el.id, x.data, x.layout, x.config);
      instance.plotly = true;
      if (x.config.mathjax == "cdn" && !instance.mathjax) {
        var script = document.createElement("script");
        script.type = "text/javascript";
        script.src  = "https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_SVG";
        document.getElementsByTagName("head")[0].appendChild(script);
        instance.mathjax = true;
      }
    } else {
      Plotly.newPlot(el.id, x.data, x.layout);
    }
  }
  
});
