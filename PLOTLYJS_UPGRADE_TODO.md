# plotly.js Upgrade TODO

Items to address following the upgrade from v2.11.1 to v2.25.2.

---

## Potential Regressions to Test

These changes in plotly.js could potentially cause regressions in existing R package functionality. Each should be tested to ensure the R package continues to work as expected.

### HIGH PRIORITY

- [ ] **Pie chart rotation behavior** (v2.15.0)
  - Change: Updated pie chart `rotation` to use `angle` value type
  - Risk: Existing R code using pie chart rotation may behave differently
  - Test: Create pie charts with rotation values and compare before/after behavior
  - Ref: https://github.com/plotly/plotly.js/releases/tag/v2.15.0

- [ ] **Selection event changes** (v2.13.0, v2.13.1, v2.13.2, v2.13.3)
  - Change: Major overhaul of selection handling - persistent/editable selections, `plotly_selected` event now emits on API calls and GUI edits
  - Risk: Shiny event handlers relying on selection events may receive different/additional events
  - Test: Test `event_data("plotly_selected")` in Shiny apps, ensure brushing behavior is unchanged
  - Ref: https://github.com/plotly/plotly.js/releases/tag/v2.13.0

- [ ] **SVG export scaling** (v2.18.1)
  - Change: Removed vector-effect CSS for static plots to fix SVG export scaling
  - Risk: Static image export via kaleido/orca may produce different results
  - Test: Export plots to SVG and compare rendering
  - Ref: https://github.com/plotly/plotly.js/releases/tag/v2.18.1

### MEDIUM PRIORITY

- [ ] **Multiple legend defaults** (v2.22.0, v2.24.2, v2.24.3)
  - Change: Added multiple legend support; several fixes to legend group/trace order defaults
  - Risk: Existing plots with legendgroups may have different default ordering
  - Test: Create plots with multiple legend groups and verify appearance
  - Ref: https://github.com/plotly/plotly.js/releases/tag/v2.22.0

- [ ] **Automargin behavior changes** (v2.13.0, v2.15.0)
  - Change: More flexible `automargin` control on cartesian axes; fixed automargin axis title updates
  - Risk: Plot margins may differ slightly from previous versions
  - Test: Run visual regression tests on plots with long axis labels/titles
  - Ref: https://github.com/plotly/plotly.js/releases/tag/v2.13.0

- [ ] **Hover label overlap fix** (v2.19.0)
  - Change: Fixed hover label overlap for `hovermode: 'x'` or `'y'`
  - Risk: Hover label positioning may change for existing plots
  - Test: Create plots with multiple traces and verify hover labels don't overlap
  - Ref: https://github.com/plotly/plotly.js/releases/tag/v2.19.0

### LOW PRIORITY

- [ ] **Bundle size changes** (v2.17.0)
  - Change: Switched bundler from browserify to webpack
  - Risk: Bundle size may have changed; performance characteristics may differ
  - Test: Compare bundle sizes, check for any performance regressions
  - Ref: https://github.com/plotly/plotly.js/releases/tag/v2.17.0

- [ ] **Static plot interactions** (v2.15.0, v2.16.5)
  - Change: Disabled interactions for static plot modes in treemap, icicle, sunburst; disabled slider interactions when staticPlot is true
  - Risk: `config(staticPlot = TRUE)` may behave differently
  - Test: Verify static plots are truly non-interactive
  - Ref: https://github.com/plotly/plotly.js/releases/tag/v2.15.0

- [ ] **Heatmap rendering changes** (v2.21.0, v2.23.0, v2.23.1)
  - Change: Fixed heatmap `zsmooth: "fast"` rendering/performance; improved rendering when zsmooth is false; fixed iOS/Safari rendering
  - Risk: Visual differences in heatmap rendering
  - Test: Run visual tests on heatmaps with different zsmooth settings
  - Ref: https://github.com/plotly/plotly.js/releases/tag/v2.21.0

---

## Potential Improvements / New Features

These are new plotly.js features that could be exposed or better supported in the R package. Not critical, but would enhance functionality.

### Legend Improvements

- [ ] **Multiple legends** (v2.22.0)
  - Feature: Support for `legend2`, `legend3`, etc. with separate positioning
  - R Impact: Could document how to use multiple legends via `layout()`
  - Priority: Medium - significant new capability
  - Ref: https://github.com/plotly/plotly.js/releases/tag/v2.22.0

- [ ] **Legend visibility control** (v2.22.0)
  - Feature: Added `visible` option for legends
  - R Impact: Could add to `layout()` documentation
  - Priority: Low
  - Ref: https://github.com/plotly/plotly.js/releases/tag/v2.22.0

- [ ] **Legend entry width** (v2.15.0)
  - Feature: `entrywidth` and `entrywidthmode` for legend items
  - R Impact: Document in legend customization examples
  - Priority: Low
  - Ref: https://github.com/plotly/plotly.js/releases/tag/v2.15.0

- [ ] **Shape legends** (v2.25.0)
  - Feature: Legends can now include shapes and `newshape`
  - R Impact: Could enhance shape documentation
  - Priority: Low
  - Ref: https://github.com/plotly/plotly.js/releases/tag/v2.25.0

### Axis/Layout Features

- [ ] **Grid dash styling** (v2.12.0)
  - Feature: `griddash` axis property for cartesian, polar, smith, ternary, geo, carpet
  - R Impact: Document new grid styling options
  - Priority: Low
  - Ref: https://github.com/plotly/plotly.js/releases/tag/v2.12.0

- [ ] **Minor tick styling** (v2.12.0)
  - Feature: `minor.tickmode`, `minor.tickvals`, `minor.tickcolor`, etc.
  - R Impact: Could add helper functions or documentation for minor tick customization
  - Priority: Low
  - Ref: https://github.com/plotly/plotly.js/releases/tag/v2.12.0

- [ ] **Y-axis shift/autoshift** (v2.17.0)
  - Feature: `shift` and `autoshift` properties to avoid y-axis overlapping
  - R Impact: Document for multi-y-axis plots
  - Priority: Medium - useful for complex plots
  - Ref: https://github.com/plotly/plotly.js/releases/tag/v2.17.0

- [ ] **Sync tickmode** (v2.18.0)
  - Feature: New `sync` tickmode option for synchronized axes
  - R Impact: Document for subplots with shared axes
  - Priority: Low
  - Ref: https://github.com/plotly/plotly.js/releases/tag/v2.18.0

- [ ] **Label alias** (v2.19.0)
  - Feature: `labelalias` for various axes types
  - R Impact: Could simplify axis label customization
  - Priority: Medium - useful feature
  - Ref: https://github.com/plotly/plotly.js/releases/tag/v2.19.0

- [ ] **Title automargin** (v2.20.0)
  - Feature: `title.automargin` for automatic top/bottom margining
  - R Impact: Could reduce need for manual margin adjustments
  - Priority: Low
  - Ref: https://github.com/plotly/plotly.js/releases/tag/v2.20.0

### Trace/Chart Features

- [ ] **Violin quartile methods** (v2.13.0)
  - Feature: Additional quartile computing algorithms for violin traces
  - R Impact: Could expose new quartile method options in violin trace helpers
  - Priority: Medium - enhances existing functionality
  - Ref: https://github.com/plotly/plotly.js/releases/tag/v2.13.0

- [ ] **Marker angle properties** (v2.15.0)
  - Feature: `marker.angle`, `marker.angleref`, `marker.standoff`
  - R Impact: Could enhance marker customization documentation or helpers
  - Priority: Medium - useful for directional data
  - Ref: https://github.com/plotly/plotly.js/releases/tag/v2.15.0

- [ ] **Scatter grouping** (v2.17.0)
  - Feature: `alignmentgroup` and `offsetgroup` for scatter traces
  - R Impact: Could enhance grouped scatter plot support
  - Priority: Medium - useful for grouped comparisons
  - Ref: https://github.com/plotly/plotly.js/releases/tag/v2.17.0

- [ ] **Sankey arrows** (v2.14.0)
  - Feature: Support for sankey links with arrows
  - R Impact: Document new sankey link options
  - Priority: Low
  - Ref: https://github.com/plotly/plotly.js/releases/tag/v2.14.0

- [ ] **Treemap corner radius** (v2.17.0)
  - Feature: `marker.cornerradius` for treemap trace
  - R Impact: Document styling option
  - Priority: Low
  - Ref: https://github.com/plotly/plotly.js/releases/tag/v2.17.0

- [ ] **Pattern support expansion** (v2.24.0)
  - Feature: Pattern support for pie, funnelarea, sunburst, icicle, and treemap charts
  - R Impact: Document pattern fill options for these chart types
  - Priority: Low
  - Ref: https://github.com/plotly/plotly.js/releases/tag/v2.24.0

### Shape Features

- [ ] **Shape labels** (v2.19.0, v2.21.0)
  - Feature: Shape `label` attribute and `label.texttemplate` for parametric shapes
  - R Impact: Could enhance shape annotation helpers
  - Priority: Medium - commonly requested feature
  - Ref: https://github.com/plotly/plotly.js/releases/tag/v2.19.0

### Geo/Map Features

- [ ] **Equal Earth projection** (v2.25.0)
  - Feature: New "Equal Earth" projection for geo subplots
  - R Impact: Document new projection option for `plot_geo()`
  - Priority: Low
  - Ref: https://github.com/plotly/plotly.js/releases/tag/v2.25.0

- [ ] **Scattermapbox clustering** (v2.16.0)
  - Feature: Clustering options and bounds support for scattermapbox
  - R Impact: Document clustering for `plot_mapbox()`
  - Priority: Medium - useful for large datasets
  - Ref: https://github.com/plotly/plotly.js/releases/tag/v2.16.0

### Selection/Interaction Features

- [ ] **Editable selections** (v2.13.0, v2.14.0)
  - Feature: Persistent and editable selections; `editSelection` config option
  - R Impact: Could expose via `config()` and document for Shiny apps
  - Priority: Medium - enhances interactivity
  - Ref: https://github.com/plotly/plotly.js/releases/tag/v2.13.0

### Indicator Features

- [ ] **Delta prefix/suffix** (v2.13.0)
  - Feature: `delta.prefix` and `delta.suffix` for indicator traces
  - R Impact: Document for indicator trace customization
  - Priority: Low
  - Ref: https://github.com/plotly/plotly.js/releases/tag/v2.13.0

### Parallel Coordinates

- [ ] **Unselected line styling** (v2.13.0)
  - Feature: Unselected line color/opacity options
  - R Impact: Document for parallel coordinates customization
  - Priority: Low
  - Ref: https://github.com/plotly/plotly.js/releases/tag/v2.13.0

---

## Testing Checklist

Before merging the upgrade:

- [ ] Run full test suite: `devtools::test()`
- [ ] Run visual tests: `Sys.setenv("VISUAL_TESTS" = "true"); devtools::test()`
- [ ] Run R CMD check: `rcmdcheck::rcmdcheck()`
- [ ] Test basic Shiny integration with event_data()
- [ ] Manually test key plot types: scatter, bar, pie, heatmap, geo, mapbox
- [ ] Compare bundle size before/after
- [ ] Test static image export via kaleido

---

## Notes

- The IE9/phantomjs compatibility patch (001-revert-ie-fallbacks.patch) continues to apply successfully through all versions
- Security fix in v2.25.2 addresses prototype pollution - important for production use
- Many locale updates throughout the versions (Dutch, French, Norwegian, Slovak, Croatian, etc.)
