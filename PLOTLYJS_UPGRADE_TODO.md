# plotly.js Upgrade TODO

Items to address following the upgrade from v2.11.1 to v2.25.2.

---

## Potential Regressions - Investigation Results

All potential regressions were investigated. **None are actual regressions** - they are all bug fixes, performance improvements, or additive features.

### Investigated Items (All Clear)

1. **Pie chart rotation behavior** (v2.15.0) - **NOT A REGRESSION**
   - Change: Schema now uses `angle` value type for `rotation` attribute
   - Finding: This is a schema-level change for type consistency. The R package doesn't use pie rotation. No behavioral change.

2. **Selection event changes** (v2.13.x) - **NOT A REGRESSION**
   - Change: `plotly_selected` now emits on API calls and GUI edits; persistent/editable selections added
   - Finding: This is an improvement - events are now emitted more consistently. May result in more events in Shiny apps (which is desired behavior).

3. **SVG export scaling** (v2.18.1) - **NOT A REGRESSION**
   - Change: Removed `vector-effect` CSS from static plots
   - Finding: This is a bug fix that improves SVG export quality. Beneficial change.

4. **Multiple legend defaults** (v2.22.0+) - **NOT A REGRESSION**
   - Change: Added multiple legend support (legend2, legend3, etc.)
   - Finding: Additive feature, doesn't change defaults. Legend toggle regression from v2.22.0 was already fixed in v2.24.2.

5. **Automargin behavior** (v2.13.0, v2.15.0) - **NOT A REGRESSION**
   - Change: Added flaglist options for granular automargin control
   - Finding: Additive feature only. Existing `automargin = TRUE` continues to work.

6. **Hover label overlap** (v2.19.0) - **NOT A REGRESSION**
   - Change: Fixed overlap of point and axis hover labels for `hovermode: 'x'|'y'`
   - Finding: Bug fix that improves hover label positioning. Beneficial change.

7. **Bundle size** (v2.17.0 webpack switch) - **NOT A REGRESSION**
   - Change: Switched bundler from browserify to webpack
   - Finding: Bundle size actually **decreased** by ~90KB (3.67MB → 3.58MB). Improvement.

8. **Static plot interactions** (v2.15.0, v2.16.5) - **NOT A REGRESSION**
   - Change: Disabled interactions for treemap, icicle, sunburst, pie, funnelarea, parcats, parcoords, sankey when `staticPlot = TRUE`
   - Finding: Bug fix ensuring static plots are truly non-interactive. Expected behavior.

9. **Heatmap rendering** (v2.21.0, v2.23.0, v2.23.1) - **NOT A REGRESSION**
   - Change: Fixed rendering bugs and improved performance for `zsmooth: "fast"` and `zsmooth: false`
   - Finding: Bug fixes and performance improvements. Visual output should be same or better.

---

## Recommended Testing Before Merge

While no regressions were identified, the following testing is recommended:

- [ ] Run full test suite: `devtools::test()`
- [ ] Run visual tests: `Sys.setenv("VISUAL_TESTS" = "true"); devtools::test()`
- [ ] Run R CMD check: `rcmdcheck::rcmdcheck()`

### Manual Review (Optional)
These require manual verification if you want extra confidence:

- [ ] Test basic Shiny integration with `event_data("plotly_selected")` to verify selection events work as expected
- [ ] Visually verify a heatmap with `zsmooth = "fast"` renders correctly
- [ ] Verify a static plot (`config(staticPlot = TRUE)`) has no interactions on pie/sunburst/treemap

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

- [ ] **Legend entry width** (v2.15.0)
  - Feature: `entrywidth` and `entrywidthmode` for legend items
  - R Impact: Document in legend customization examples
  - Priority: Low

- [ ] **Shape legends** (v2.25.0)
  - Feature: Legends can now include shapes and `newshape`
  - R Impact: Could enhance shape documentation
  - Priority: Low

### Axis/Layout Features

- [ ] **Grid dash styling** (v2.12.0)
  - Feature: `griddash` axis property for cartesian, polar, smith, ternary, geo, carpet
  - R Impact: Document new grid styling options
  - Priority: Low

- [ ] **Minor tick styling** (v2.12.0)
  - Feature: `minor.tickmode`, `minor.tickvals`, `minor.tickcolor`, etc.
  - R Impact: Could add helper functions or documentation for minor tick customization
  - Priority: Low

- [ ] **Y-axis shift/autoshift** (v2.17.0)
  - Feature: `shift` and `autoshift` properties to avoid y-axis overlapping
  - R Impact: Document for multi-y-axis plots
  - Priority: Medium - useful for complex plots

- [ ] **Sync tickmode** (v2.18.0)
  - Feature: New `sync` tickmode option for synchronized axes
  - R Impact: Document for subplots with shared axes
  - Priority: Low

- [ ] **Label alias** (v2.19.0)
  - Feature: `labelalias` for various axes types
  - R Impact: Could simplify axis label customization
  - Priority: Medium - useful feature

- [ ] **Title automargin** (v2.20.0)
  - Feature: `title.automargin` for automatic top/bottom margining
  - R Impact: Could reduce need for manual margin adjustments
  - Priority: Low

### Trace/Chart Features

- [ ] **Violin quartile methods** (v2.13.0)
  - Feature: Additional quartile computing algorithms for violin traces
  - R Impact: Could expose new quartile method options in violin trace helpers
  - Priority: Medium - enhances existing functionality

- [ ] **Marker angle properties** (v2.15.0)
  - Feature: `marker.angle`, `marker.angleref`, `marker.standoff`
  - R Impact: Could enhance marker customization documentation or helpers
  - Priority: Medium - useful for directional data

- [ ] **Scatter grouping** (v2.17.0)
  - Feature: `alignmentgroup` and `offsetgroup` for scatter traces
  - R Impact: Could enhance grouped scatter plot support
  - Priority: Medium - useful for grouped comparisons

- [ ] **Sankey arrows** (v2.14.0)
  - Feature: Support for sankey links with arrows
  - R Impact: Document new sankey link options
  - Priority: Low

- [ ] **Treemap corner radius** (v2.17.0)
  - Feature: `marker.cornerradius` for treemap trace
  - R Impact: Document styling option
  - Priority: Low

- [ ] **Pattern support expansion** (v2.24.0)
  - Feature: Pattern support for pie, funnelarea, sunburst, icicle, and treemap charts
  - R Impact: Document pattern fill options for these chart types
  - Priority: Low

### Shape Features

- [ ] **Shape labels** (v2.19.0, v2.21.0)
  - Feature: Shape `label` attribute and `label.texttemplate` for parametric shapes
  - R Impact: Could enhance shape annotation helpers
  - Priority: Medium - commonly requested feature

### Geo/Map Features

- [ ] **Equal Earth projection** (v2.25.0)
  - Feature: New "Equal Earth" projection for geo subplots
  - R Impact: Document new projection option for `plot_geo()`
  - Priority: Low

- [ ] **Scattermapbox clustering** (v2.16.0)
  - Feature: Clustering options and bounds support for scattermapbox
  - R Impact: Document clustering for `plot_mapbox()`
  - Priority: Medium - useful for large datasets

### Selection/Interaction Features

- [ ] **Editable selections** (v2.13.0, v2.14.0)
  - Feature: Persistent and editable selections; `editSelection` config option
  - R Impact: Could expose via `config()` and document for Shiny apps
  - Priority: Medium - enhances interactivity

### Indicator Features

- [ ] **Delta prefix/suffix** (v2.13.0)
  - Feature: `delta.prefix` and `delta.suffix` for indicator traces
  - R Impact: Document for indicator trace customization
  - Priority: Low

### Parallel Coordinates

- [ ] **Unselected line styling** (v2.13.0)
  - Feature: Unselected line color/opacity options
  - R Impact: Document for parallel coordinates customization
  - Priority: Low

---

## Notes

- The IE9/phantomjs compatibility patch (001-revert-ie-fallbacks.patch) continues to apply successfully through all versions
- Security fix in v2.25.2 addresses prototype pollution - important for production use
- Many locale updates throughout the versions (Dutch, French, Norwegian, Slovak, Croatian, etc.)
- Bundle size decreased from v2.11.1 to v2.25.2 (~90KB reduction due to webpack optimization)
