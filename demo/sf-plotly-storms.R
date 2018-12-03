library(plotly)

storms <- sf::st_read(system.file("shape/storms_xyz.shp", package = "sf"), quiet = TRUE)
stormz <- highlight_key(storms)

xy <- plot_ly(stormz, color = ~z, mode = "markers+lines", line = list(color = "gray"), hoverinfo = "none")
xz <- add_sf(xy, y = ~z, color = ~y)
yz <- add_sf(xy, x = ~z, color = ~x)
xyz2D <- subplot(xy, xz, yz, titleX = TRUE, titleY = TRUE) %>%
  hide_legend() %>%
  hide_colorbar() %>%
  highlight(selected = attrs_selected(hoverinfo = "x+y"))

xyz3D <- plot_ly(stormz, color = ~z, z = ~z) %>%
  # this bit rotates the 3D graph
  htmlwidgets::onRender(
    "function(el, x) {
    
    var gd = document.getElementById(el.id); 
    
    var cnt = 0;
    
    function run() {
    rotate('scene', Math.PI / 180);
    requestAnimationFrame(run);
    } 
    run();
    
    function rotate(id, angle) {
    var scene = gd._fullLayout[id]._scene;
    var camera = scene.getCamera();
    
    var rtz = xyz2rtz(camera.eye);
    
    rtz.t += angle;
    
    camera.eye = rtz2xyz(rtz);
    
    scene.setCamera(camera);
    }
    
    // Math.atan2 will rotate the full 360, but it doesn't render for some reason?
    function xyz2rtz(xyz) {
    return {
    r: Math.sqrt(xyz.x * xyz.x + xyz.y * xyz.y),
    t: Math.atan(xyz.y / xyz.x),
    z: xyz.z
    };
    }
    
    function rtz2xyz(rtz) {
    return {
    x: rtz.r * Math.cos(rtz.t),
    y: rtz.r * Math.sin(rtz.t),
    z: rtz.z
    };
    }
    
    }"
  )


library(htmltools)
browsable(tagList(xyz2D, xyz3D))
