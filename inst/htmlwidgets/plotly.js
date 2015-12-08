
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
    
    var g = x.data[0].set;
    var grp = crosstalk.group(g);
    
    $('#'+el.id).on('plotly_click', function(event, data) {
      // extract only the data we may want to access in R
      var d = data.points.map(function(pt) {
        var obj = {
            curveNumber: pt.curveNumber, 
            pointNumber: pt.pointNumber, 
            x: pt.x,
            y: pt.y
        };
        if (pt.data.hasOwnProperty("key")) {
          if (typeof pt.pointNumber === "number") {
            obj.key = pt.data.key[pt.pointNumber];
          } else {
            obj.key = pt.data.key[pt.pointNumber[0]][pt.pointNumber[1]];
          } // TODO: can pointNumber be 3D?
        }
        return obj;
      });
      
      // tell crosstalk about click data so we can access it in R (and JS)
      grp.var("plotly_click").set(d);
      // TODO: provide visual clue that we've selected point(s)
      // (it's possible this will be handled natively in plotlyjs...)
    });
      
    grp.var('plotly_click').on('change', function(e) {
      console.log(e.value);
      // TODO: if e.value.x !=  e.oldValue.x, then newPlot? Otherwise, restyle?
    });
    
  }
  
});
