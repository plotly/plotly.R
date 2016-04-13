
HTMLWidgets.widget({
  name: "plotly",
  type: "output",
  
  initialize: function(el, width, height) {
    return {};
  },
  
  resize: function(el, width, height, instance) {
    if (instance.autosize) {
      Plotly.relayout(el.id, {width: width, height: height});
    }
  },  
  
  renderValue: function(el, x, instance) {
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
      instance.autosize = x.layout.autosize;
    } else {
      // Can we do smooth transitions of x/y locations of points?
      var doTransition = x.data.length === graphDiv._fullData.length;
      for (i = 0; i < x.data.length; i++) {
        doTransition = doTransition && 
          x.data[i].x.length === graphDiv._fullData[i].x.length &&
          x.data[i].y.length === graphDiv._fullData[i].y.length &&
          //x.data[i].type || "scatter" === "scatter";
          x.data[i].mode === "markers";
      }
      
      if (!doTransition) {
        Plotly.newPlot(graphDiv, x.data, x.layout);
      } else {
        // construct x/y scales from graphDiv
        var lay = graphDiv._fullLayout;
        var xDom = lay.xaxis.range;
        var yDom = lay.yaxis.range;
        var xRng = [0, lay.width - lay.margin.l - lay.margin.r];
        var yRng = [lay.height - lay.margin.t - lay.margin.b, 0];
        // TODO: does this generalize to non-linear scales?
        var xaxis = Plotly.d3.scale.linear().domain(xDom).range(xRng);
        var yaxis = Plotly.d3.scale.linear().domain(yDom).range(yRng);
        // store new x/y positions as an array of objects (for D3 bind)
        x.transitionDat = [];
        for (i = 0; i < x.data.length; i++) {
          var d = x.data[i];
          for (j = 0; j < d.x.length; j++) {
            x.transitionDat.push({x: d.x[j], y: d.y[j]});
          }
        }
        // we call newPlot() with old x/y locations and transition to new ones
        for (i = 0; i < x.data.length; i++) {
          x.data[i].xNew = x.data[i].x;
          x.data[i].yNew = x.data[i].y;
          x.data[i].x = graphDiv._fullData[i].x;
          x.data[i].y = graphDiv._fullData[i].y;
        }
        // attempt to transition when appropriate
        window.requestAnimationFrame(function() {
          var pts = Plotly.d3.selectAll('.scatterlayer .point');
          if (doTransition && pts[0].length > 0) {
            // Transition the transform -- 
            // https://gist.github.com/mbostock/1642874
            pts
              .data(x.transitionDat)
            .transition()
            // TODO: how to provide arguments to these options?
              .duration(0)
              .ease('linear')
              .attr('transform', function(d) { 
                return 'translate(' + xaxis(d.x) + ',' + yaxis(d.y) + ')';
              });
            // update the graphDiv data
            for (i = 0; i < x.data.length; i++) {
              graphDiv._fullData[i].x = x.data[i].xNew;
              graphDiv._fullData[i].y = x.data[i].yNew;
            }
          }
        });
      }
      
    }
    
    sendEventData = function(eventType) {
      return function(eventData) {
        if (eventData === undefined || !eventData.hasOwnProperty("points")) {
          return null;
        }
        var d = eventData.points.map(function(pt) {
          var obj = {
                curveNumber: pt.curveNumber, 
                pointNumber: pt.pointNumber, 
                x: pt.x,
                y: pt.y
          };
          // grab the trace corresponding to this point
          var tr = x.data[pt.curveNumber];
          // add on additional trace info, if it exists
          attachKey = function(keyName) {
            if (tr.hasOwnProperty(keyName) && tr[keyName] !== null) {
              if (typeof pt.pointNumber === "number") {
                obj[keyName] = tr[keyName][pt.pointNumber];
              } else {
                obj[keyName] = tr[keyName][pt.pointNumber[0]][pt.pointNumber[1]];
              }// TODO: can pointNumber be 3D?
            }
          };
          attachKey("z");
          attachKey("key");
          return obj; 
        });
        Shiny.onInputChange(
          ".clientValue-" + eventType + "-" + x.source, 
          JSON.stringify(d)
        );
      };
    };
    
    // send user input event data to shiny
    if (shinyMode) {
      // https://plot.ly/javascript/zoom-events/
      graphDiv.on('plotly_relayout', function(d) {
        Shiny.onInputChange(
          ".clientValue-" + "plotly_relayout" + "-" + x.source, 
          JSON.stringify(d)
        );
      });
      graphDiv.on('plotly_hover', sendEventData('plotly_hover'));
      graphDiv.on('plotly_click', sendEventData('plotly_click'));
      graphDiv.on('plotly_selected', sendEventData('plotly_selected'));
      graphDiv.on('plotly_unhover', function(eventData) {
        Shiny.onInputChange(".clientValue-plotly_hover-" + x.source, null);
      });
      graphDiv.on('plotly_doubleclick', function(eventData) {
        Shiny.onInputChange(".clientValue-plotly_click-" + x.source, null);
      });
      // 'plotly_deselect' is code for doubleclick when in select mode
      graphDiv.on('plotly_deselect', function(eventData) {
        Shiny.onInputChange(".clientValue-plotly_selected-" + x.source, null);
        Shiny.onInputChange(".clientValue-plotly_click-" + x.source, null);
      });
    } 
    
  } 
  
});
