
HTMLWidgets.widget({
  name: "plotly",
  type: "output",

  initialize: function(el, width, height) {
    return {
      // Push JavaScript closures onto this list, and renderValue
      // will pop them off and run them one at a time the next
      // time it runs. Use this to dispose of e.g. old event
      // registrations.
      onNextRender: []
    };
  },

  resize: function(el, width, height, instance) {
    if (instance.autosize) {
      Plotly.relayout(el.id, {width: width, height: height});
    }
  },

  renderValue: function(el, x, instance) {

    // Release previously registered crosstalk event listeners
    while (instance.onNextRender.length > 0) {
      instance.onNextRender.pop()();
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
      instance.autosize = x.layout.autosize;
    } else {
      Plotly.newPlot(graphDiv, x.data, x.layout);
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
    
    // # Begin Crosstalk support
    
    // ## Crosstalk point/key translation functions
    //
    // Plotly.js uses curveNumber/pointNumber addressing to refer
    // to data points (i.e. when plotly_selected is received). We
    // prefer to let the R user use key-based addressing, where
    // a string is used that uniquely identifies a data point (we
    // can also use row numbers in a pinch).
    //
    // The pointsToKeys and keysToPoints functions let you convert
    // between the two schemes.
    
    // To allow translation from keys to points in O(1) time, we
    // make a cache that lets us map keys to objects with
    // {curveNumber, pointNumber} properties.
    var keyCache = {};
    for (var curve = 0; curve < x.data.length; curve++) {
      if (!x.data[curve].key) {
        continue;
      }
      for (var pointIdx = 0; pointIdx < x.data[curve].key.length; pointIdx++) {
        keyCache[x.data[curve].key[pointIdx]] = {curveNumber: curve, pointNumber: pointIdx};
      }
    }
    
    // Given an array of {curveNumber: x, pointNumber: y} objects,
    // return an array of key strings.
    // TODO: Throw proper error if any point is invalid or doesn't
    // have a key?
    function pointsToKeys(points) {
      var keys = [];
      for (var i = 0; i < points.length; i++) {
        // Look up the keys
        var key = graphDiv.data[points[i].curveNumber].key[points[i].pointNumber];
        keys.push(key);
      }
      return keys;
    }
    
    // Given an array of strings, return an object that hierarchically
    // represents the corresponding curves/points.
    //
    // For example, the following data:
    //
    // [
    //   {curveNumber: 0, pointNumber: 1},
    //   {curveNumber: 0, pointNumber: 2},
    //   {curveNumber: 2, pointNumber: 1}
    // ]
    //
    // would be returned as:
    // {
    //   "0": [1, 2],
    //   "2": [1]
    // }
    function keysToPoints(keys) {
      var curves = {};
      for (var i = 0; i < keys.length; i++) {
        var pt = keyCache[keys[i]];
        if (!pt) {
          throw new Error("Unknown key " + keys[i]);
        }
        curves[pt.curveNumber] = curves[pt.curveNumber] || [];
        curves[pt.curveNumber].push(pt.pointNumber);
      }
      return curves;
    }

    // Grab the specified crosstalk group.
    if (x.set) {
      var grp = crosstalk.group(x.set);
      
      // When plotly selection changes, update crosstalk
      graphDiv.on("plotly_selected", function plotly_selecting(e) {
        if (e) {
          var selectedKeys = pointsToKeys(e.points);
          grp.var("selection").set(selectedKeys, {sender: el});
        }
      });
      // When plotly selection is cleared, update crosstalk
      graphDiv.on("plotly_deselect", function plotly_deselect(e) {
        grp.var("selection").set(null);
      });
      
      // When crosstalk selection changes, update plotly style
      grp.var("selection").on("change", function crosstalk_sel_change(e) {
        // e.value is either null, or an array of newly selected values
        
        if (e.sender !== el) {
          // If we're not the originator of this selection, and we have an
          // active selection outline box, we need to remove it. Otherwise
          // it could appear like there are two active brushes in one plot
          // group.
          var outlines = el.querySelectorAll(".select-outline");
          for (var i = 0; i < outlines.length; i++) {
            outlines[i].remove();
          }
        }
        
        // Restyle each relevant trace
        var selectedPoints = keysToPoints(e.value || []);
        
        var opacityTraces = [];
        var traceIndices = [];
        
        for (var i = 0; i < x.data.length; i++) {
          if (!x.data[i].key) {
            // Not a brushable trace apparently. Don't restyle.
            continue;
          }
          
          // Make an opacity array, one element for each data point
          // in this trace.
          var opacity = new Array(x.data[i].x.length);
          
          if (typeof(e.value) === "undefined" || e.value === null) {
            // The Crosstalk selection has been cleared. Full opacity
            for (var k = 0; k < opacity.length; k++) {
              opacity[k] = 1;
            }
          } else {
            // Array of pointNumber numbers that should be highlighted
            var theseSelectedPoints = selectedPoints[i] || [];
            
            for (var j = 0; j < opacity.length; j++) {
              if (theseSelectedPoints.indexOf(j) >= 0) {
                opacity[j] = 1;
              } else {
                opacity[j] = 0.2;
              }
            }
          }
          
          opacityTraces.push(opacity);
          traceIndices.push(i);
        }
        // Restyle the current trace
        Plotly.restyle(graphDiv, {"marker.opacity": opacityTraces}, traceIndices);
      });
      
      // Remove event listeners in the future
      instance.onNextRender.push(function() {
        graphDiv.removeListener("plotly_selecting", plotly_selecting);
        graphDiv.removeListener("plotly_deselect", plotly_deselect);
        grp.removeListener("selection", crosstalk_sel_change);
      });
    }
    
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
