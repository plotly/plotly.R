
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
    
    // Combine the name of a set and key into a single string, suitable for
    // using as a keyCache key.
    function joinSetAndKey(set, key) {
      return set + "\n" + key;
    }
    
    // To allow translation from sets+keys to points in O(1) time, we
    // make a cache that lets us map keys to objects with
    // {curveNumber, pointNumber} properties.
    var keyCache = {};
    for (var curve = 0; curve < x.data.length; curve++) {
      var curveObj = x.data[curve];
      if (!curveObj.key || !curveObj.set) {
        continue;
      }
      for (var pointIdx = 0; pointIdx < curveObj.key.length; pointIdx++) {
        keyCache[joinSetAndKey(curveObj.set, curveObj.key[pointIdx])] =
          {curveNumber: curve, pointNumber: pointIdx};
      }
    }
    
    // Given an array of {curveNumber: x, pointNumber: y} objects,
    // return a hash of {[set1]: [key1, key2, ...], [set2]: [key3, key4, ...]}
    function pointsToKeys(points) {
      var keysBySet = {};
      for (var i = 0; i < points.length; i++) {
        var curveObj = graphDiv.data[points[i].curveNumber];
        if (!curveObj.key || !curveObj.set) {
          // If this curve isn't mapped to a set, ignore this point.
          continue;
        }
        // Look up the keys
        var key = curveObj.key[points[i].pointNumber];
        keysBySet[curveObj.set] = keysBySet[curveObj.set] || [];
        keysBySet[curveObj.set].push(key);
      }
      return keysBySet;
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
    function keysToPoints(set, keys) {
      var curves = {};
      for (var i = 0; i < keys.length; i++) {
        var pt = keyCache[joinSetAndKey(set, keys[i])];
        if (!pt) {
          throw new Error("Unknown key " + keys[i]);
        }
        curves[pt.curveNumber] = curves[pt.curveNumber] || [];
        curves[pt.curveNumber].push(pt.pointNumber);
      }
      return curves;
    }
    
    // Gather all sets.
    var crosstalkGroups = {};
    var allSets = [];
    for (var curveIdx = 0; curveIdx < x.data.length; curveIdx++) {
      if (x.data[curveIdx].set) {
        if (!crosstalkGroups[x.data[curveIdx].set]) {
          allSets.push(x.data[curveIdx].set);
          crosstalkGroups[x.data[curveIdx].set] = [];
        }
        crosstalkGroups[x.data[curveIdx].set].push(curveIdx);
      }
    }

    if (allSets.length > 0) {
      // When plotly selection changes, update crosstalk
      graphDiv.on("plotly_selected", function plotly_selecting(e) {
        if (e) {
          var selectedKeys = pointsToKeys(e.points);
          // Keys are group names, values are array of selected keys from group.
          for (var set in selectedKeys) {
            if (selectedKeys.hasOwnProperty(set))
              crosstalk.group(set).var("selection").set(selectedKeys[set], {sender: el});
          }
          // Any groups that weren't represented in the selection, should be
          // treated as if zero points were selected.
          for (var i = 0; i < allSets.length; i++) {
            if (!selectedKeys[allSets[i]]) {
              crosstalk.group(allSets[i]).var("selection").set([], {sender: el});
            }
          }
        }
      });
      // When plotly selection is cleared, update crosstalk
      graphDiv.on("plotly_deselect", function plotly_deselect(e) {
        for (var i = 0; i < allSets.length; i++) {
          crosstalk.group(allSets[i]).var("selection").set(null, {sender: el});
        }
      });

      for (var i = 0; i < allSets.length; i++) {
        (function() {
          var set = allSets[i];
          var grp = crosstalk.group(set);
  
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
            var selectedPoints = keysToPoints(set, e.value || []);
            
            var opacityTraces = [];
            var relevantTraces = crosstalkGroups[set];

            for (var i = 0; i < relevantTraces.length; i++) {
              var trace = x.data[relevantTraces[i]];

              // Make an opacity array, one element for each data point
              // in this trace.
              var opacity = new Array(trace.x.length);
              
              if (typeof(e.value) === "undefined" || e.value === null) {
                // The Crosstalk selection has been cleared. Full opacity
                for (var k = 0; k < opacity.length; k++) {
                  opacity[k] = 1;
                }
              } else {
                // Array of pointNumber numbers that should be highlighted
                var theseSelectedPoints = selectedPoints[relevantTraces[i]] || [];
                
                for (var j = 0; j < opacity.length; j++) {
                  if (theseSelectedPoints.indexOf(j) >= 0) {
                    opacity[j] = 1;
                  } else {
                    opacity[j] = 0.2;
                  }
                }
              }
              
              opacityTraces.push(opacity);
            }
            console.log(graphDiv.id, relevantTraces, opacityTraces)
            // Restyle the current trace
            Plotly.restyle(graphDiv, {"marker.opacity": opacityTraces}, relevantTraces);
          });
  
          // Remove event listeners in the future
          instance.onNextRender.push(function() {
            grp.removeListener("selection", crosstalk_sel_change);
          });
        })();
      }
      
      // Remove event listeners in the future
      instance.onNextRender.push(function() {
        graphDiv.removeListener("plotly_selecting", plotly_selecting);
        graphDiv.removeListener("plotly_deselect", plotly_deselect);
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
