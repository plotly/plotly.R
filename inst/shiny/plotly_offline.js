var binding = new Shiny.OutputBinding();

binding.find = function(scope) {
    return $(scope).find('.plotly_offline');
};

binding.renderValue = function(el, dat) {
  Plotly.plot(dat[0]["id"], dat[0]["data"], dat[0]["layout"])
};

Shiny.outputBindings.register(binding, "plotlyOffline");
