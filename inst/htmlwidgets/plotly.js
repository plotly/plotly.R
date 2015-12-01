
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
    
    // TODO: pass in group from R!
    var ctgrp = crosstalk.group('grp');
    
    console.log(x.elementId);
    dat = x.data;
    
    // was a selection key specified?
    if (x.data[0].hasOwnProperty("key")) {
      // TODO: pass in event from R!
      $('#'+el.id).on('plotly_click', function(event, data) {
        
        pts = data.points;
        keys = data.points.map(function(pt){
          return pt.data.key[pt.pointNumber];
        });
        ctgrp.var("tdb").set(keys);
      });
      
      ctgrp.var("tdb").on('change', function(e) {
        console.log(e.value);
        
        var opacity = x.data.map(function(tr) {
           var o = tr.key.map(function(k) {
             // TODO: what is the JS equivalent of %in% ?
             if (k == e.value[0]) return 1; else return 0.2;
           });
           return o;
        });
       
        op = {"opacity": opacity};
        
        Plotly.restyle(el, op);
      });
      
    }
    
  }
  
});


/*
        // decrease the opacity of every point
        var op = [];
        for (var i = 0; i < x.data.length; i++) {
           var d = x.data[i];
           var n = d.key.length;
           // TODO: smart preservation of specified opacity?
           var o = [];
           for (j = 0; j < n; j++) {
             o.push(0.2);
           }
           op.push(o);
        }
        
        // increase the opacity of clicked point(s)
        for (var k = 0; k < data.points.length; k++) {
          var pt = data.points[k];
          op[pt.curveNumber][pt.pointNumber] = 1;
        }
        
        var opacity = {"opacity": op};
        
        ctgrp.var("tdb").set(opacity);
        */
        
