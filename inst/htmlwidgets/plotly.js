
HTMLWidgets.widget({
  name: "plotly",
  type: "output",

  initialize: function(el, width, height) {
    // when upgrading plotly.js,
    // uncomment this console.log(), then do `load_all(); plot_ly()` 
    // open in chrome, right-click on console output: "save-as" -> "schema.json"
    // Schema <- jsonlite::fromJSON("~/Downloads/schema.json")
    // devtools::use_data(Schema, overwrite = T, internal = T)
    // console.log(JSON.stringify(Plotly.PlotSchema.get()));
    
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
      var width = instance.width || width;
      var height = instance.height || height;
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
      
      var plot = Plotly.plot(graphDiv, x);
      instance.plotly = true;
      instance.autosize = x.layout.autosize || true;
      instance.width = x.layout.width;
      instance.height = x.layout.height;
      
    } else {
      
      var plot = Plotly.newPlot(graphDiv, x);
      
    }
    
    // Attach attributes (e.g., "key", "z") to plotly event data
    function eventDataWithKey(eventData) {
      if (eventData === undefined || !eventData.hasOwnProperty("points")) {
        return null;
      }
      return eventData.points.map(function(pt) {
        var obj = {
          curveNumber: pt.curveNumber, 
          pointNumber: pt.pointNumber, 
          x: pt.x,
          y: pt.y
        };
        /* 
          TL;DR: (I think) we have to select the graph div (again) to attach keys...
          
          Why? Remember that crosstalk will dynamically add/delete traces 
          (see traceManager.prototype.updateSelection() below)
          For this reason, we can't simply grab keys from x.data (like we did previously)
          Moreover, we can't use _fullData, since that doesn't include 
          unofficial attributes. It's true that click/hover events fire with 
          pt.data, but drag events don't...
        */
        var gd = document.getElementById(el.id);
        var trace = gd.data[pt.curveNumber];
        // Add other attributes here, if desired
        var attrsToAttach = ["key", "z"];
        for (var i = 0; i < attrsToAttach.length; i++) {
          var attr = trace[attrsToAttach[i]];
          if (Array.isArray(attr)) {
              // pointNumber can be an array (e.g., heatmaps)
              // TODO: can pointNumber be 3D?
              obj[attrsToAttach[i]] = typeof pt.pointNumber === "number" ? 
                attr[pt.pointNumber] : attr[pt.pointNumber[0]][pt.pointNumber[1]];
          }
        }
        return obj;
      });
    }
    
    // send user input event data to shiny
    if (shinyMode) {
      // https://plot.ly/javascript/zoom-events/
      graphDiv.on('plotly_relayout', function(d) {
        Shiny.onInputChange(
          ".clientValue-plotly_relayout-" + x.source, 
          JSON.stringify(eventDataWithKey(d))
        );
      });
      graphDiv.on('plotly_hover', function(d) {
        Shiny.onInputChange(
          ".clientValue-plotly_hover-" + x.source, 
          JSON.stringify(eventDataWithKey(d))
        );
      });
      graphDiv.on('plotly_click', function(d) {
        Shiny.onInputChange(
          ".clientValue-plotly_click-" + x.source, 
          JSON.stringify(eventDataWithKey(d))
        );
      });
      graphDiv.on('plotly_selected', function(d) {
        Shiny.onInputChange(
          ".clientValue-plotly_selected-" + x.source, 
          JSON.stringify(eventDataWithKey(d))
        );
      });
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
      var key = [];
      if (typeof(curveObj.key) === "object") {
        key.push(Object.keys(curveObj.key));
      } else {
        key.push(curveObj.key);
      }
      for (var pointIdx = 0; pointIdx < key.length; pointIdx++) {
        keyCache[joinSetAndKey(curveObj.set, key[pointIdx])] =
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
    
    x.highlight.color = x.highlight.color || [];
    // make sure highlight color is an array
    if (!Array.isArray(x.highlight.color)) {
      x.highlight.color = [x.highlight.color];
    }

    var traceManager = new TraceManager(graphDiv, x.highlight);

    // Gather all *unique* sets.
    var allSets = [];
    for (var curveIdx = 0; curveIdx < x.data.length; curveIdx++) {
      var newSet = x.data[curveIdx].set;
      if (newSet) {
        if (allSets.indexOf(newSet) === -1) {
          allSets.push(newSet);
        }
      }
    }

    if (allSets.length > 0) {
      
      // On plotly event, update crosstalk variable selection value
      graphDiv.on(x.highlight.on, function turnOn(e) {
        if (e) {
          var selectedKeys = pointsToKeys(e.points);
          // Keys are group names, values are array of selected keys from group.
          for (var set in selectedKeys) {
            if (selectedKeys.hasOwnProperty(set)) {
              crosstalk.group(set).var("selection")
                .set(selectedKeys[set], {sender: el});
            }
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
      
      // On a plotly "clear" event, set crosstalk variable value to null
      graphDiv.on(x.highlight.off, function turnOff(e) {
        for (var i = 0; i < allSets.length; i++) {
          crosstalk.group(allSets[i]).var("selection").set(null, {sender: el});
        }
      });
      

      for (var i = 0; i < allSets.length; i++) {
        (function() {
          var set = allSets[i];
          var grp = crosstalk.group(set);
          
          // Create a selectize widget for each group
          if (x.selectize) {
            var selectizeID = Object.keys(x.selectize)[i];
            var items = x.selectize[selectizeID].items;
            var first = [{value: "", label: "(All)"}];
            var opts = {
              options: first.concat(items),
              searchField: "label",
              valueField: "value",
              labelField: "label"
            };
            var select = $("#" + selectizeID).find("select")[0];
            var selectize = $(select).selectize(opts)[0].selectize;
            selectize.on("change", function() {
              // TODO: updateSelection() should really work when we *remove* items...
              traceManager.updateSelection(set, selectize.items);
            });
          }
          
          // When crosstalk selection changes, update plotly style
          grp.var("selection").on("change", function crosstalk_sel_change(e) {
            if (e.sender !== el) {
              // If we're not the originator of this selection, and we have an
              // active selection outline box, we need to remove it. Otherwise
              // it could appear like there are two active brushes in one plot
              // group.
              removeBrush(el);
            }
            
            // e.value is either null, or an array of newly selected values
            if (e.oldValue !== e.value) {
              traceManager.updateSelection(set, e.value);
              // https://github.com/selectize/selectize.js/blob/master/docs/api.md#methods_items
              if (x.selectize) {
                if (e.value === null) {
                  selectize.clear(true);
                } else {
                  selectize.addItem(e.value, true);
                  selectize.close();
                }
              }
              
            }
          });

          grp.var("filter").on("change", function crosstalk_filter_change(e) {
            traceManager.updateFilter(set, e.value);
          });
  
          /* Remove event listeners in the future
          CPS: for some reason I was getting crosstalk_sel_change is undefined in a shiny app?
          instance.onNextRender.push(function() {
            grp.var("selection").removeListener("change", crosstalk_sel_change);
            grp.var("filter").removeListener("change", crosstalk_filter_change);
          });
          */
        })();
      }
    }
  }
});

/**
 * @param graphDiv The Plotly graph div
 * @param highlight An object with options for updating selection(s)
 */
function TraceManager(graphDiv, highlight) {
  // The Plotly graph div
  this.gd = graphDiv;

  // Preserve the original data. We'll subset based off of this whenever
  // filtering is (re)applied.
  this.origData = JSON.parse(JSON.stringify(graphDiv.data));
  
  // to avoid doing this over and over
  this.origOpacity = [];
  for (var i = 0; i < this.origData.length; i++) {
    this.origOpacity[i] = this.origData[i].opacity || 1;
  }

  // key: group name, value: null or array of keys representing the
  // most recently received selection for that group.
  this.groupSelections = {};
  
  // selection parameters (e.g., transient versus persistent selection)
  this.highlight = highlight;
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
  
    for (var i = 0; i < this.origData.length; i++) {
      var trace = this.origData[i];
      if (!trace.key || trace.set !== group) {
        continue;
      }
      var matches = findNestedMatches(trace.key, keys);
      // subsetArrayAttrs doesn't mutate trace (it makes a modified clone)
      this.gd.data[i] = subsetArrayAttrs(trace, matches);
    }
  }

  Plotly.redraw(this.gd);
  
  // NOTE: we purposely do _not_ restore selection(s), since on filter,
  // axis likely will update, changing the pixel -> data mapping, leading 
  // to a likely mismatch in the brush outline and highlighted marks
  
};

TraceManager.prototype.updateSelection = function(group, keys) {
  
  if (keys !== null && !Array.isArray(keys)) {
    throw new Error("Invalid keys argument; null or array expected");
  }
  
  this.groupSelections[group] = keys;
  
  // if selection has been cleared, or if this is transient (not persistent)
  // selection, delete the "selection traces"
  var nNewTraces = this.gd.data.length - this.origData.length;
  if (keys === null || !this.highlight.persistent && nNewTraces > 0) {
    var tracesToRemove = [];
    for (var i = this.origData.length; i < this.gd.data.length; i++) {
      tracesToRemove.push(i);
    }
    Plotly.deleteTraces(this.gd, tracesToRemove);
  }
  
  if (keys === null) {
    
    // go back to original opacity
    Plotly.restyle(this.gd, {"opacity": this.origOpacity});
    
  } else if (keys.length >= 1) {
    
    // placeholder for new "selection traces"
    var traces = [];
    // this variable is set in R/highlight.R
    var selectionColour = crosstalk.var("plotlySelectionColour").get() || 
      this.highlight.color[0];
    
    // TODO: think about how we could set a mode in the dynamic brush to just highlight 
    // (i.e., this mode would preserve coloring of existing selections and simply dim opacity)
    var isTransparent = selectionColour === "transparent";
    if (isTransparent) {
      selectionColour = null;
    }
    
    
    for (var i = 0; i < this.origData.length; i++) {
      var trace = this.gd.data[i];
      if (!trace.key || trace.set !== group) {
        continue;
      }
      // Get sorted array of matching indices in trace.key
      var matches = findNestedMatches(trace.key, keys);
      if (matches.length > 0) {
        trace = subsetArrayAttrs(trace, matches);
        trace.opacity = this.origOpacity[i];
        trace.showlegend = this.highlight.showInLegend;
        trace.hoverinfo = this.highlight.hoverinfo || trace.hoverinfo;
        trace.name = "selected";
        // inherit marker/line attributes from the existing trace
        var idx = (isTransparent) ? trace._newIndex || i : i
        var d = this.gd._fullData[idx];
        if (d.marker) {
          trace.marker = d.marker;
          trace.marker.color =  selectionColour || trace.marker.color;
        }
        if (d.line) {
          trace.line = d.line;
          trace.line.color =  selectionColour || trace.line.color;
        }
        if (d.textfont) {
          trace.textfont = d.textfont;
          trace.textfont.color =  selectionColour || trace.textfont.color;
        }
        // keep track of mapping between this new trace and the trace it targets
        trace._originalIndex = i;
        trace._newIndex = this.gd._fullData.length + traces.length;
        //this.gd.data[i]._newIndex = this.gd._fullData.length + traces.length;
        traces.push(trace);
      }
    }
    
    if (traces.length > 0) {
      
      // dim original traces that have a set matching the set of selection sets
      var sets = Object.keys(this.groupSelections);
      // transparent selections also allow for dimming of selection sets
      var n = isTransparent ? this.gd.data.length : this.origData.length;
      for (var i = 0; i < n; i++) {
        var opacity = this.origOpacity[i] || 1;
        // have we already dimmed this trace?
        //if (opacity !== this.gd._fullData[i].opacity && selectionColour !== "transparent") {
        //  continue;
        //}
        // is this worth doing?
        if (this.highlight.opacityDim === 1) {
          continue;
        }
        var matches = findNestedMatches(sets, [this.gd.data[i].set]);
        if (matches.length) {
          Plotly.restyle(this.gd, {"opacity": opacity * this.highlight.opacityDim}, i);
        }
      }
      
      var nCurrentTraces = this.gd._fullData.length;
      
      Plotly.addTraces(this.gd, traces).then(function(gd) {
        // incrementally add selection traces to frames
        // (this is heavily inspired by Plotly.Plots.modifyFrames() 
        // in src/plots/plots.js)
        var _hash = gd._transitionData._frameHash;
        var _frames = gd._transitionData._frames || [];
        
        for (var i = 0; i < _frames.length; i++) {
          
          // add to _frames[i].traces *if* this frame references selected trace(s)
          var newIndices = [];
          for (var j = 0; j < traces.length; j++) {
            var tr = traces[j];
            if (_frames[i].traces.indexOf(tr._originalIndex) > -1) {
              newIndices.push(tr._newIndex);
              _frames[i].traces.push(tr._newIndex);
            }
          }
          
          // nothing to do...
          if (newIndices.length === 0) {
            continue;
          }
          
          var ctr = 0;
          var nFrameTraces = _frames[i].data.length;
          
          for (var j = 0; j < nFrameTraces; j++) {
            var frameTrace = _frames[i].data[j];
            if (!frameTrace.key || frameTrace.set !== group) {
              continue;
            }
            // Get sorted array of matching indices in trace.key
            var matches = findNestedMatches(frameTrace.key, keys);
            if (matches.length > 0) {
              frameTrace = subsetArrayAttrs(frameTrace, matches);
              var d = gd._fullData[newIndices[ctr]];
              if (d.marker) {
                frameTrace.marker = d.marker;
              }
              if (d.line) {
                frameTrace.line = d.line;
              }
              if (d.textfont) {
                frameTrace.textfont = d.textfont;
              }
              ctr = ctr + 1;
              _frames[i].data.push(frameTrace);
            }
          }
          
          // update gd._transitionData._frameHash
          _hash[_frames[i].name] = _frames[i];
        }
      
      });
      
    }
    
  }
};


// find matches for nested keys
function findNestedMatches(haystack, needleSet) {
  
  // ensure haystack is an array of an arrays
  if (!Array.isArray(haystack)) {  
    haystack = [haystack];
  }
  for (var i = 0; i < haystack.length; i++) {
    if (!Array.isArray(haystack[i])) {
      haystack[i] = [haystack[i]];
    }
  }
  // ensure needleSet is an array of an arrays
  if (!Array.isArray(needleSet)) {  
    needleSet = [needleSet]; 
  }
  for (var i = 0; i < needleSet.length; i++) {
    if (!Array.isArray(needleSet[i])) {
      needleSet[i] = [needleSet[i]];
    }
  }
  
  // return a match if a haystack array is a superset of a needleset array
  var matches = [];
  for (var i = 0; i < haystack.length; i++) {
    var hay = haystack[i];
    for (var j = 0; j < needleSet.length; j++) {
      // have we already found a match?
      if (matches.indexOf(i) >= 0) {
        continue;
      }
      var match = hay.every(function(val) { 
        return val === null || needleSet[j].indexOf(val) >= 0; 
      });
      if (match) {
        matches.push(i);
      }
    }
  }
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

// Convenience function for removing plotly's brush 
function removeBrush(el) {
  var outlines = el.querySelectorAll(".select-outline");
  for (var i = 0; i < outlines.length; i++) {
    outlines[i].remove();
  }
}
