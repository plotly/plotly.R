
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
    
    // TODO: pass in group from R?
    var ctgrp = crosstalk.group('grp');
    
    // check if a source interaction is specified for this plot
    if (typeof x.source === "string" || x.source instanceof String) {
      var sources = x.source.split("+");
      // TODO: pass in event from R?
      $('#'+el.id).bind('plotly_click', function(event, data) {
        // let crosstalk know about the selection
        for (var i = 0; i < sources.length; i++) {
          var j = sources[i];
          ctgrp.var(j).set(data.points[0][j]);
        }
        // TODO: how to visually modify this thing we've selected?
        // note that 'this' is actually the whole widget
      });
    }
    
    // check if a target interaction is specified for this plot
    if (typeof x.target === "string" || x.target instanceof String) {
      var tgt = x.target.split("+");
      for (var i = 0; i < tgt.length; i++) {
        var j = tgt[i];
        ctgrp.var(j).on('change', function(e) {
          var b = x.data[0][j].filter(function(d) { return d == e.value; }); 
          console.log(b);
          //Plotly.addTraces(x.elementId, b);
        });
      }
      
    }
    
  }
  
});
