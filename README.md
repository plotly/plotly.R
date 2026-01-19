This is a fork of the plotly/plotly.R HtmlWidget for use in Displayr. The repository has been modified to be an node package and use the rhtmlBuildUtils framework. This was done in order to run unit tests on the widget js code, which has been modified by Displayr. The integration with rhtmlBuildUtils is incomplete, and only the building and unit testing of widget js code is supported. Features from rhtmlBuildUtils such as visual regression testing, linting and the internal web server are absent.

This fork diverges from the plotly/plotly.R in that
* Uses a later version of the js bundle (2.35.2) compared to the upstream fork (2.11.1). In particular this allows us to use bar corner radius
* Add support for handling the QTable class
* Some changes that so that plotly runs tests and exports in the Displayr environment

This fork is also missing some of the more recent fixes for ggplotly. The latest version of ggplot2 is 3.5.1 but as of version 3.5.0 (released Feb 2024), there have been a number of breaking changes. the Displayr R server still uses 3.4.4. 
