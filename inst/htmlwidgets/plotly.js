
HTMLWidgets.widget({
  name: "plotly",
  type: "output",
  
  initialize: function(el, width, height) {
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
    
    var ctgrp = crosstalk.group(x.set);
    
    // was a selection key specified?
    if (x.data[0].hasOwnProperty("key")) {
      // TODO: pass in event from R!
      $('#'+el.id).on('plotly_click', function(event, data) {
        
        var keys = data.points.map(function(pt){
          return pt.data.key[pt.pointNumber];
        });
        
        ctgrp.var("tdb").set(keys);
      });
      
      ctgrp.var("tdb").on('change', function(e) {
        
        var opacity = x.data.map(function(tr) {
           var o = tr.key.map(function(k) {
             // TODO: what is the JS equivalent of %in% ?
             if (k == e.value[0]) return 1; else return 0.2;
           });
           return o;
        });
       
        op = {"marker.opacity": opacity};
        
        Plotly.restyle(el, op);
      });
      
    }
    
  }
  
});
