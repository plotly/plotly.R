
HTMLWidgets.widget({
  name: "plotly",
  type: "output",
  
  initialize: function(el, width, height) {
    return {};
  },
  
  resize: function(el, width, height, instance) {
    // TODO: impose fixed coordinates, if specified (see #342)
    Plotly.relayout(el.id, {width: width, height: height});
  },  
  
  renderValue: function(el, x, instance) {
    // make sure plots don't get created outside the network
    window.PLOTLYENV = window.PLOTLYENV || {};
    window.PLOTLYENV.BASE_URL = x.base_url;
    
    var graphDiv = document.getElementById(el.id);
    
    // if no plot exists yet, create one with a particular configuration
    if (!instance.plotly) {
      Plotly.plot(graphDiv, x.data, x.layout, x.config);
      instance.plotly = true;
    } else {
      Plotly.newPlot(graphDiv, x.data, x.layout);
    }
    
    var g = x.data[0].set;
    var grp = crosstalk.group(g);
    
    graphDiv.on('plotly_click', function(eventData) {
      // extract only the data we may want to access in R
      var d = eventData.points.map(function(pt) {
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
    });
      
    graphDiv.on('plotly_selected', function(eventData) {
      if (eventData !== undefined) {
        // convert the array of objects to object of arrays so this converts
        // to data frame in R as opposed to a vector
        var pts = eventData.points;
        var obj = {
          curveNumber: pts.map(function(pt) {return pt.curveNumber; }),
          pointNumber: pts.map(function(pt) {return pt.pointNumber; }), 
          x: pts.map(function(pt) {return pt.x; }),
          y: pts.map(function(pt) {return pt.y; })
        };
        grp.var("plotly_selected").set(obj);
      }
    });  
      
  }
  
});
