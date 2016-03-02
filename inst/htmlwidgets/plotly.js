
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
    
    // map date/datetimes (both in milliseconds) to date objects
    if (x.layout.xaxis.isDate === true) {
      x.data[0].x = x.data[0].x.map(function(i) { return new Date(i); });
      x.layout.xaxis.tickformat = '%Y';
      x.layout.xaxis.hoverformat = '%Y';
    }
    
    var shinyMode;
    if (typeof(window) !== "undefined") {
      // make sure plots don't get created outside the network
      window.PLOTLYENV = window.PLOTLYENV || {};
      window.PLOTLYENV.BASE_URL = x.base_url;
      shinyMode = !!window.Shiny;
    }
    
    var graphDiv = document.getElementById(el.id);
    
    // if no plot exists yet, create one with a particular configuration
    if (!instance.plotly) {
      Plotly.plot(graphDiv, x.data, x.layout, x.config);
      instance.plotly = true;
    } else {
      Plotly.newPlot(graphDiv, x.data, x.layout);
    }
    
    // send user input event data to shiny
    if (shinyMode) {
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
        Shiny.onInputChange(".clientValue-plotly_click-" + x.source, d);
      });
      
      // clear click selection
      graphDiv.on('plotly_doubleclick', function(eventData) {
        Shiny.onInputChange(".clientValue-plotly_click-" + x.source, null);
      });
      
      graphDiv.on('plotly_hover', function(eventData) {
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
        Shiny.onInputChange(".clientValue-plotly_hover-" + x.source, d);
      });
      
      // clear hover selection
      graphDiv.on('plotly_unhover', function(eventData) {
        Shiny.onInputChange(".clientValue-plotly_hover-" + x.source, null);
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
          Shiny.onInputChange(".clientValue-plotly_selected-" + x.source, obj);
        }
      });
      
      // clear select/lasso selection & click
      graphDiv.on('plotly_deselect', function(eventData) {
        Shiny.onInputChange(".clientValue-plotly_selected-" + x.source, null);
        Shiny.onInputChange(".clientValue-plotly_click-" + x.source, null);
      });
      
    } // shinyMode
  } // renderValue
  
});
