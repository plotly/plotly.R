# plotly.js Upgrade Log

Tracking the upgrade of plotly.js from v2.11.1 to v2.25.2.

## Versions to Upgrade (28 total)

1. 2.12.0
2. 2.12.1
3. 2.13.0
4. 2.13.1
5. 2.13.2
6. 2.13.3
7. 2.14.0
8. 2.15.0
9. 2.15.1
10. 2.16.0
11. 2.16.1
12. 2.16.2
13. 2.16.3
14. 2.16.4
15. 2.16.5
16. 2.17.0
17. 2.17.1
18. 2.18.0
19. 2.18.1
20. 2.18.2
21. 2.19.0
22. 2.19.1
23. 2.20.0
24. 2.21.0
25. 2.22.0
26. 2.23.0
27. 2.23.1
28. 2.23.2
29. 2.24.0
30. 2.24.1
31. 2.24.2
32. 2.24.3
33. 2.25.0
34. 2.25.1
35. 2.25.2

## Upgrade Progress

### v2.12.0
- **Release Notes**: https://github.com/plotly/plotly.js/releases/tag/v2.12.0
- **Status**: Complete
- **Key Changes**:
  - Added `griddash` axis property for multiple plot types (cartesian, polar, smith, ternary, geo, carpet)
  - Added minor tick and grid line styling options (minor.tickmode, minor.tickvals, minor.tickcolor, etc.)
  - Performance improvement: Used "willReadFrequently" 2D context creation attribute
  - Fixed blank tick labels on cartesian axes
- **Issues/Concerns**: None - additive changes only

---

### v2.12.1
- **Release Notes**: https://github.com/plotly/plotly.js/releases/tag/v2.12.1
- **Status**: Complete
- **Key Changes**:
  - Fixed disabling polar rotation when `dragmode` is false
  - Fixed custom modebar buttons mutating input
  - Corrected missing and duplicate spaces in plot schema descriptions
- **Issues/Concerns**: None - bug fixes only

---

### v2.13.0
- **Release Notes**: https://github.com/plotly/plotly.js/releases/tag/v2.13.0
- **Status**: Complete
- **Key Changes**:
  - Persistent and editable selections over cartesian subplots
  - New parallel coordinates options (unselected line color/opacity)
  - Enhanced violin trace with additional quartile computing algorithms
  - More flexible `automargin` control on cartesian axes
  - Added `delta.prefix` and `delta.suffix` to indicator traces
  - Added Chinese (Taiwan) and Sinhala locales
  - Updated modebar logo
- **Issues/Concerns**:
  - New selection features may need testing
  - Consider exposing new violin quartile methods in R

---

### v2.13.1
- **Release Notes**: https://github.com/plotly/plotly.js/releases/tag/v2.13.1
- **Status**: Complete
- **Key Changes**:
  - Fixed regression where selections were improperly attached to undefined event data
- **Issues/Concerns**: None - bug fix only

---

### v2.13.2
- **Release Notes**: https://github.com/plotly/plotly.js/releases/tag/v2.13.2
- **Status**: Complete
- **Key Changes**:
  - Fixed sankey select error introduced in v2.13.0
  - Fixed handling of missing drag layer for invisible sankey traces
  - Fixed selection event emission in shape drawing dragmodes
- **Issues/Concerns**: None - bug fixes only

---

### v2.13.3
- **Release Notes**: https://github.com/plotly/plotly.js/releases/tag/v2.13.3
- **Status**: Complete
- **Key Changes**:
  - Emit `plotly_selected` event on plot API calls and GUI edits
- **Issues/Concerns**: None - bug fix only

---

### v2.14.0
- **Release Notes**: https://github.com/plotly/plotly.js/releases/tag/v2.14.0
- **Status**: Complete
- **Key Changes**:
  - Added support for sankey links with arrows
  - Added `editSelection` option to config
  - Updated Dutch translations and fixed dateMonth format for 'nl' locale
- **Issues/Concerns**: None - additive changes only

---

### v2.15.0
- **Release Notes**: https://github.com/plotly/plotly.js/releases/tag/v2.15.0
- **Status**: Complete
- **Key Changes**:
  - New marker properties: `angle`, `angleref`, `standoff`
  - Legend control: `entrywidth` and `entrywidthmode`
  - Layout: `minreducedwidth` and `minreducedheight` for automargin control
  - Updated pie chart `rotation` to use `angle` value type
  - Fixed automargin axis title updates
  - Fixed pattern and slice export issues
  - Disabled interactions for static plot modes in treemap, icicle, sunburst, etc.
- **Issues/Concerns**:
  - New marker angle properties may need R wrapper support
  - Pie chart rotation change might affect existing code

---

### v2.15.1
- **Release Notes**: https://github.com/plotly/plotly.js/releases/tag/v2.15.1
- **Status**: Complete
- **Key Changes**:
  - Fix for npm module issue
- **Issues/Concerns**: None - npm packaging fix only

---

### v2.16.0
- **Release Notes**: https://github.com/plotly/plotly.js/releases/tag/v2.16.0
- **Status**: Complete
- **Key Changes**:
  - Clustering options for `scattermapbox`
  - Bounds support for mapbox subplots
- **Issues/Concerns**: None - new mapbox features

---

### v2.16.1
- **Release Notes**: https://github.com/plotly/plotly.js/releases/tag/v2.16.1
- **Status**: Complete
- **Key Changes**:
  - Fixed choroplethmapbox selection when adding new traces
- **Issues/Concerns**: None - bug fix only

---

### v2.16.2
- **Release Notes**: https://github.com/plotly/plotly.js/releases/tag/v2.16.2
- **Status**: Complete
- **Key Changes**:
  - Fixed regression in mapbox clearOutline from v2.13.0
- **Issues/Concerns**: None - bug fix only

---

### v2.16.3
- **Release Notes**: https://github.com/plotly/plotly.js/releases/tag/v2.16.3
- **Status**: Complete
- **Key Changes**:
  - Fixed hover on multicategory axes
- **Issues/Concerns**: None - bug fix only

---

### v2.16.4
- **Release Notes**: https://github.com/plotly/plotly.js/releases/tag/v2.16.4
- **Status**: Complete
- **Key Changes**:
  - Fixed regression with scattermapbox redraw from v2.16.0
- **Issues/Concerns**: None - bug fix only

---

### v2.16.5
- **Release Notes**: https://github.com/plotly/plotly.js/releases/tag/v2.16.5
- **Status**: Complete
- **Key Changes**:
  - Disabled slider interactions when staticPlot is true
- **Issues/Concerns**: None - bug fix only

---

### v2.17.0
- **Release Notes**: https://github.com/plotly/plotly.js/releases/tag/v2.17.0
- **Status**: Complete
- **Key Changes**:
  - Added y-axes `shift` and `autoshift` to avoid axis overlapping
  - Added group attributes for scatter trace: `alignmentgroup`, `offsetgroup`
  - Added `marker.cornerradius` for treemap trace
  - Switched bundler from browserify to webpack
  - Fixed auto backoff for marker symbols and sizes arrays
- **Issues/Concerns**:
  - Webpack bundler change - watch for bundle size changes
  - New scatter grouping attributes may need R wrapper support

---

### v2.17.1
- **Release Notes**: https://github.com/plotly/plotly.js/releases/tag/v2.17.1
- **Status**: Complete
- **Key Changes**:
  - Fixed line redraw regression from v2.15.0
- **Issues/Concerns**: None - bug fix only

---

### v2.18.0
- **Release Notes**: https://github.com/plotly/plotly.js/releases/tag/v2.18.0
- **Status**: Complete
- **Key Changes**:
  - Added new `sync` tickmode option
  - Improved mobile & tablet device detection for WebGL rendering
  - Fixed requirejs AMD loader import name issue (regression from v2.17.0)
- **Issues/Concerns**: None

---

### v2.18.1
- **Release Notes**: https://github.com/plotly/plotly.js/releases/tag/v2.18.1
- **Status**: Complete
- **Key Changes**:
  - Upgraded d3-interpolate and d3-color to v3 (audit warnings)
  - Fixed SVG export scaling by removing vector-effect CSS for static plots
  - Fixed hover on IE (regression from v2.5.0)
- **Issues/Concerns**: None

---

### v2.18.2
- **Release Notes**: https://github.com/plotly/plotly.js/releases/tag/v2.18.2
- **Status**: Complete
- **Key Changes**:
  - Prevented attaching internal d3 object to window (regression from v2.17.0)
  - Fixed lower/upper fence order in French locale
  - Fixed formats in Peruvian Spanish (es-pe) locale
- **Issues/Concerns**: None

---

### v2.19.0
- **Release Notes**: https://github.com/plotly/plotly.js/releases/tag/v2.19.0
- **Status**: Complete
- **Key Changes**:
  - Added shape `label` attribute
  - Added `labelalias` for various axes types
  - Fixed hover label overlap for hovermode: 'x'|'y'
- **Issues/Concerns**:
  - New shape label feature may need R wrapper support

---

### v2.19.1
- **Release Notes**: https://github.com/plotly/plotly.js/releases/tag/v2.19.1
- **Status**: Complete
- **Key Changes**:
  - Ensure slider range stays in bounds during drag
- **Issues/Concerns**: None - bug fix only

---

### v2.20.0
- **Release Notes**: https://github.com/plotly/plotly.js/releases/tag/v2.20.0
- **Status**: Complete
- **Key Changes**:
  - Added `title.automargin` for automatic top/bottom margining
- **Issues/Concerns**: None

---

### v2.21.0
- **Release Notes**: https://github.com/plotly/plotly.js/releases/tag/v2.21.0
- **Status**: Complete
- **Key Changes**:
  - Added `label.texttemplate` for parametric shapes
  - Added strict option for custom bundle command
  - Fixed legend dragging with non-default anchors
  - Fixed heatmap zsmooth: "fast" rendering/performance
- **Issues/Concerns**: None

---

### v2.22.0
- **Release Notes**: https://github.com/plotly/plotly.js/releases/tag/v2.22.0
- **Status**: Complete
- **Key Changes**:
  - Multiple legend support with legend2, legend3, etc.
  - Added `visible` option for legends
  - Fixed plotly_click on touch devices for gl3d scenes
  - Fixed scatter3d when marker.opacity is zero
  - Fixed scattermapbox visibility restyle
  - Updated Norwegian and Slovak translations
- **Issues/Concerns**:
  - Multiple legends is a significant feature - may need R wrapper support

---

### v2.23.0
- **Release Notes**: https://github.com/plotly/plotly.js/releases/tag/v2.23.0
- **Status**: Complete
- **Key Changes**:
  - Added legend.xref and legend.yref for container-referenced positioning
  - Added colorbar.xref and colorbar.yref for container-referenced positioning
  - Improved heatmap rendering performance when zsmooth is false
- **Issues/Concerns**: None

---

### v2.23.1
- **Release Notes**: https://github.com/plotly/plotly.js/releases/tag/v2.23.1
- **Status**: Complete
- **Key Changes**:
  - Fixed heatmap rendering on iOS and Safari when zsmooth is false
- **Issues/Concerns**: None - bug fix only

---

### v2.23.2
- **Release Notes**: https://github.com/plotly/plotly.js/releases/tag/v2.23.2
- **Status**: Complete
- **Key Changes**:
  - Fixed text rendering while drawing new shapes
- **Issues/Concerns**: None - bug fix only

---

