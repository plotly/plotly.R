
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
      var plot = Plotly.plot(graphDiv, x.data, x.layout, x.config);
      instance.plotly = true;
      instance.autosize = x.layout.autosize;
    } else {
      var plot = Plotly.newPlot(graphDiv, x.data, x.layout);
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

    var traceManager = new TraceManager(graphDiv);

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
      var turnOn = function(e) {
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
      };
      
      // gather all the unique "on" event types
      var onEvents = [];
      for (var i = 0; i < x.data.length; i++) {
        var evt = x.data[i].crosstalk.on;
        for (var j = 0; j < evt.length; j++) {
          if (onEvents.indexOf(evt[j]) === -1) {
            onEvents.push(evt[j]);
          }
        }
      }
      
      // register a callback for every type
      for (var i = 0; i < onEvents.length; i++) {
        graphDiv.on(onEvents[i], turnOn);
      }
      
      // gather all the unique "off" event types
      // TODO: eliminate this copy pasta
      var offEvents = [];
      for (var i = 0; i < x.data.length; i++) {
        var evt = x.data[i].crosstalk.off;
        for (var j = 0; j < evt.length; j++) {
          if (offEvents.indexOf(evt[j]) === -1) {
            offEvents.push(evt[j]);
          }
        }
      }
      
      var turnOff = function(e) {
        for (var i = 0; i < allSets.length; i++) {
          crosstalk.group(allSets[i]).var("selection").set(null, {sender: el});
        }
      };
      
      // When plotly selection is cleared, update crosstalk
      graphDiv.on(offEvents, turnOff);

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
            if (e.oldValue !== e.value) {
              traceManager.updateSelection(set, e.value);
            }
          });

          grp.var("filter").on("change", function crosstalk_filter_change(e) {
            traceManager.updateFilter(set, e.value);
          });
  
          // Remove event listeners in the future
          instance.onNextRender.push(function() {
            grp.var("selection").removeListener("change", crosstalk_sel_change);
            grp.var("filter").removeListener("change", crosstalk_filter_change);
          });
        })();
      }
      
      // Remove event listeners in the future
      instance.onNextRender.push(function() {
        graphDiv.removeListener("plotly_selecting", plotly_selecting);
        graphDiv.removeListener("plotly_deselect", plotly_deselect);
      });
    }
/*    
    // TODO: modify event_data() to use crosstalk's R API
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
*/ 

  }

});



/**
 * @param graphDiv The Plotly graph div
 * @param group The crosstalk group object
 */
function TraceManager(graphDiv) {
  // The Plotly graph div
  this.gd = graphDiv;

  // Preserve the original data. We'll subset based off of this whenever
  // filtering is (re)applied.
  this.origData = JSON.parse(JSON.stringify(graphDiv.data));

  // key: group name, value: null or array of keys representing the
  // most recently received selection for that group.
  this.groupSelections = {};
}

TraceManager.prototype.close = function() {
  // TODO: Unhook all event handlers
};

TraceManager.prototype.updateFilter = function(group, keys) {
  // Be sure NOT to modify origData (or trace, which is a reference to
  // a child of origData).

  if (typeof(keys) === "undefined" || keys === null) {
    this.gd.data = JSON.parse(JSON.stringify(this.origData));
  } else {
    var keySet = new Set(keys);
  
    for (var i = 0; i < this.origData.length; i++) {
      var trace = this.origData[i];
      if (!trace.key || trace.set !== group) {
        continue;
      }
      var matches = findMatches(trace.key, keySet);
      // subsetArrayAttrs doesn't mutate trace (it makes a modified clone)
      this.gd.data[i] = subsetArrayAttrs(trace, matches);
    }
  }

  Plotly.redraw(this.gd);

  // If this group had a selection, restore it now
  if (this.groupSelections[group]) {
    this.updateSelection(group, this.groupSelections[group]);
  }
};

TraceManager.prototype.updateSelection = function(group, keys) {
    // NOTE: for a given selection, this is being called 4 times in examples/shiny-crosstalk/app.R (twice per group). Why is it sometimes being passed an empty array?
    console.log(keys);
  if (keys !== null && !Array.isArray(keys)) {
    throw new Error("Invalid keys argument; null or array expected");
  }
  
  // remove any existing selection traces
  // TODO: add control(s) for persistant selections?
  var tracesToRemove = [];
  for (var i = this.origData.length; i < this.gd.data.length; i++) {
    tracesToRemove.push(i);
  }
  Plotly.deleteTraces(this.gd, tracesToRemove);
  
  // selection has been cleared
  if (keys === null) {
    
    for (var i = 0; i < this.origData.length; i++) {
      // go back to original opacity
      if (this.origData[i].opacity !== this.gd.data[i].opacity) {
        Plotly.restyle(
          this.gd, {"opacity": (this.origData[i].opacity || 1)}, i
        );
      }
    }
    
  } else if (keys.length >= 1) {
    
    var keySet = new Set(keys || []);
    this.groupSelections[group] = keys;
    
    var traces = [];
    for (var i = 0; i < this.origData.length; i++) {
      var trace = this.origData[i];
      if (!trace.key || trace.set !== group) {
        continue;
      }
      var ct = trace.crosstalk || {};
      var opacity = (trace.opacity || 1) * ct.opacityDim;
      Plotly.restyle(this.gd, {"opacity": opacity}, i);
      // Get sorted array of matching indices in trace.key
      var matches = findMatches(trace.key, keySet);
      if (matches.length > 0) {
        trace = subsetArrayAttrs(trace, matches);
        trace.showlegend = ct.showInLegend;
        trace.name = "selected";
        if (ct.color) {
          trace.marker = trace.marker || this.gd._fullData[i].marker || {};
          // TODO: why is ct.color an empty array?!?
          if (typeof(ct.color) == "string") {
            trace.marker.color = ct.color;
          }
          trace.line = trace.line || this.gd._fullData[i].line || {};
          if (typeof(ct.color) == "string") {
            trace.line.color = ct.color || trace.line.color;
          }
        }
        traces.push(trace);
      }
    }
    Plotly.addTraces(this.gd, traces);
  }
};


function Set(contents /* optional */) {
  this._map = {};
  if (contents) {
    contents.forEach(this.add, this);
  }
}

Set.prototype.has = function(val) {
  return !!this._map[val];
};

Set.prototype.add = function(val) {
  this._map[val] = true;
};

Set.prototype.remove = function(val) {
  delete this._map[val];
};

function findMatches(haystack, needleSet) {
  var matches = [];
  haystack.forEach(function(obj, i) {
    if (needleSet.has(obj)) {
      matches.push(i);
    }
  });
  return matches;
}

function isPlainObject(obj) {
    return (
        Object.prototype.toString.call(obj) === '[object Object]' &&
        Object.getPrototypeOf(obj) === Object.prototype
    );
}

function subsetArrayAttrs(obj, indices) {
  var newObj = {};
  Object.keys(obj).forEach(function(k) {
    var val = obj[k];

    if (k.charAt(0) === "_") {
      newObj[k] = val;
    } else if (k === "transforms" && Array.isArray(val)) {
      newObj[k] = val.map(function(transform) {
        return subsetArrayAttrs(transform, indices);
      });
    } else if (k === "colorscale" && Array.isArray(val)) {
      newObj[k] = val;
    } else if (isPlainObject(val)) {
      newObj[k] = subsetArrayAttrs(val, indices);
    } else if (Array.isArray(val)) {
      newObj[k] = subsetArray(val, indices);
    } else {
      newObj[k] = val;
    }
  });
  return newObj;
}

function subsetArray(arr, indices) {
  var result = [];
  for (var i = 0; i < indices.length; i++) {
    result.push(arr[indices[i]]);
  }
  return result;
}
