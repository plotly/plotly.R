var binding = new Shiny.OutputBinding();

binding.find = function(scope) {
    return $(scope).find('.plotly_embed');
};

binding.renderValue = function(el, messages) {
    // this gets called when the inputs change
    // messages is the list of named lists passed up
    // by renderGraph. these named lists are postMessage
    // commands that get passed into the embedded graph.
    // See https://github.com/plotly/Embed-API for details
    var $el = $(el);

    if (!window.graphs) {
        initGraphs(messages);
    }
    messages.forEach(function(message){
        console.log('posting ', message, ' to ', message.id);
        window.graphs[message.id].graphContentWindow.postMessage(message, 'https://plot.ly');
    });

};

Shiny.outputBindings.register(binding, "plotlyEmbed");

function initGraphs(initialMessages){
    var $graphs = $('.plotly_embed');
    var graphs = {};
    $graphs.each(function(i){
        graphs[$graphs[i].id] = {
            graphContentWindow: $graphs[i].contentWindow,
            id: $graphs[i].id,
            pinger: setInterval(function(){
                $graphs[i].contentWindow.postMessage({task: 'ping'}, 'https://plot.ly');
            }, 500)
        };
    });

    // messages coming from the embedded graphs
    // either 'pong' or the 'hover', 'zoom', 'click' events
    window.addEventListener('message', function(e) {
        var message = e.data;
        for (var graph_id in graphs) {
            if (graphs[graph_id].graphContentWindow === e.source) {
                var graph = graphs[graph_id];
                break;
            }
        }

        var pinger = graph.pinger;
        var graphContentWindow = graph.graphContentWindow;
        var id = graph.id;

        if ('pong' in message && message.pong) {
            clearInterval(pinger);
            graphContentWindow.postMessage({
                'task': 'listen',
                'events': ['zoom', 'click', 'hover']},
            'https://plot.ly');
            initialMessages.forEach(function(initialMessage){
                if(initialMessage.id == id){
                    graphContentWindow.postMessage(initialMessage, 'https://plot.ly');
                }
            });
            // TODO: send pong back to R
        } else if (message.type === 'hover' ||
                    message.type === 'zoom'  ||
                    message.type === 'click') {
            if (message.type !== 'zoom') {
                for(var i in message.points) {
                    delete message.points[i].data;
                }
            }
            // TODO: Send back to R
        }
    });
    window.graphs = graphs;
}
