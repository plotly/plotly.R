library(plotly)
library(htmlwidgets)

# Thanks to Etienne Tetreault-Pinard
# http://codepen.io/etpinard/pen/jmvyxV?editors=0010

plot_ly(z = list(list(1,2,3), list(3,2,1)), type = "surface") %>%
  onRender(
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

