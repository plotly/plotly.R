{ pkgs ? import <nixpkgs> {}, displayrUtils }:

pkgs.rPackages.buildRPackage {
  name = "plotly";
  version = displayrUtils.extractRVersion (builtins.readFile ./DESCRIPTION); 
  src = ./.;
  description = "Create interactive web graphics from 'ggplot2' graphs and/or a custom interface to the (MIT-licensed) JavaScript library 'plotly.js' inspired by the grammar of graphics.";
  propagatedBuildInputs = with pkgs.rPackages; [ tools scales httr jsonlite magrittr digest viridisLite base64enc htmltools htmlwidgets tidyr RColorBrewer dplyr vctrs tibble lazyeval rlang crosstalk purrr data_table promises ];

}
