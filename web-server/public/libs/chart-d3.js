
// We assume d3 has already been loaded

function chartd3 (canvas, w, h) {

  var svg;
  svg = canvas.append("svg").attr("width", w).attr("height", h);

  this.title="";
  this.xlabel="";
  this.ylabel="";
  this.existance=1;
  this.update=true;
  this.delay = 1000; // in ms
  this.w = w;
  this.h = h;
  this.colors = [ "blue", "red", "green", "orange" ];
  this.point_width = 10;
  this.scale=true; // If true, we rescale the y-scale each update
  this.bgcolor="rgb(240,240,240)";
  this.ymax = 1;
  this.yabsmax = 0;

  /* type of plot */
  this.modell = "line";
  // types currently available are:
  //  > line
  //  > heatmap

  /* Private vars */
  yscale = 1;
  xscale = 1;
  ypad = 20;
  xpad = 50;

  // Init drawing
  svg.append("rect")
     .attr("x", 0)
     .attr("y", 20)
     .attr("rx", 5)
     .attr("ry", 5)
     .attr("width", w-5)
     .attr("height", h)
     .attr("fill", this.bgcolor);

  // CURRENTLY, support for subtraction only exists when inserting 1-width data
  // sets at a time.
  this.data_saved = {};   // data_type==1 forces subtraction of previous point
  this.data_type = {};    // (trust javascript to implement this well)
  this.key_map = [];
  this.data = {};
  var data_max_y = 0;
  var data_max_x = 0;

  // Public functions
  this.funcOne=funcOne;
  function funcOne () {
    console.log("test");

  }

  
  this.remove=remove;
  function remove() {
    svg.remove();
    delete data;
  }

  /*******************************/
  /* Data manipulation functions */
  this.addData=addData;
  function addData (key, new_data, type) {
    if (typeof(type)=="undefined") type=0;
    if (this.key_map.indexOf(key)==-1) {
      if (typeof(new_data)=="number") {
        if (type) {
          this.data_saved[key] = new_data;
          this.data[Object.keys(this.data).length] = [0];
        } else {
          this.data[Object.keys(this.data).length] = [new_data];
        }
      } else {
        this.data[Object.keys(this.data).length] = new_data;
      }
      this.key_map.push(key);
      this.data_type[key] = type;
      //if (this.data_type[this.key_map.indexOf(key)]) this.data_saved[key]=new_data
    } else {
      if (typeof(new_data)=="number") {
        if (this.data_type[key]) {
          this.data[this.key_map.indexOf(key)].push(new_data-this.data_saved[key]);
          this.data_saved[key] = new_data;
        } else {
          this.data[this.key_map.indexOf(key)].push(new_data);
        }
      } else {
        for (i in new_data) {
          this.data[this.key_map.indexOf(key)].push(new_data[i]);
        }
      }
      
    }
    
    this.draw;
  }

  //
  // Takes an input column vector and appends data to array.
  //
  this.addColumn=addColumn;
  function addColumn (new_data) {
    if (new_data.length>Object.keys(this.data).length) {
      for (var i=0; i<new_data.length-Object.keys(data).length; i++) {
        this.data[Object.keys(this.data).length] = [];
      }
    }
    for (var i in new_data) {
      this.data[i].push(new_data[i]);
    }
    
    data_max_y = Math.max(Math.max.apply(Math, new_data), data_max_y);
    this.draw();
  }

  // Returns array of all keys
  this.keys=keys;
  function keys () {
    var key_list = [];
    for (i in this.key_map) {
      key_list.push(this.key_map[i].split(','));
    }
    return key_list;
  }

  // Clears the data, but keeps the position in memory
  this.clearData=clearData;
  function clearData (group) {
    toClear = [];
    if (typeof(group) == "undefined") {
      for (var set in Object.keys(this.data)) {
        toClear.push(set);
      }
    } else if (typeof(group) == "number") {
      toClear.push(group);
    } else {
      console.log(group);
      for (set in group) {
        toClear.push(group[set]);
      }
    }
    for (var k in toClear) {
      this.data[k].length = 0;
    }
  }

  // Removes the data from the array entirely
  this.removeData=removeData;
  function removeData (group) {
    if (typeof(group) == "undefined") {
      console.log("clearing");
      console.log("all");
    } else if (typeof(group) == "number") {
      console.log("clearing")
      console.log(group);
    } else {
      console.log(group);
      for (set in group) {
        console.log("clearing");
        console.log(group[set]);
      }
    }
  }

  /**********************/
  /* Graphics functions */
  this.draw=draw;
  function draw () {
    svg.selectAll("text").remove();
    if (this.modell=="line") {
      this.draw_line_plot();
    } else if (this.modell=="heatmap") {
      this.draw_heat_map();
    } else {
      console.log("Unrecognized type.");
    }
    svg.append("text")
       .text(this.title)
       .attr("x", 18)
       .attr("y", 18)
       .attr("font-family", "sans-serif")
       .attr("font-size", 20)
       .attr("font-weight", "bold")
       .attr("fill", "black")
       .attr("text-anchor", "start");
  }

  /* Heatmap-plot drawing */
  this.draw_heat_map=draw_heat_map;
  function draw_heat_map () {
    // Clear the palate
    svg.selectAll("rect").remove();
    svg.selectAll("line").remove();
    
    // Canvas (background)
    svg.append("rect")
       .attr("x", 0)
       .attr("y", 20)
       .attr("rx", 5)
       .attr("ry", 5)
       .attr("width", this.w-5)
       .attr("height", this.h-20)
       .attr("fill", this.bgcolor);

    // Update dataset values
    data_max_y = 0;
    for (var i in this.data) {
      this.data[i].splice(0,this.data[i].length-this.point_width);
      data_max_y = Math.max(Math.max.apply(Math, this.data[i]), data_max_y);
      data_max_x = Math.max(data_max_x, this.data[i].length);
    }
    data_max_y = Math.max(this.ymax, data_max_y);
    this.yabsmax = Math.max(this.yabsmax, data_max_y);

    // Calculate scaling
    yscale = 255/this.yabsmax;
    xscale = (this.w-xpad)/this.point_width;
    vertscale = (this.h-ypad-20)/Object.keys(this.data).length;

    for (i in this.data) {
      // c'est une colonne
      sx = Math.max(this.point_width-this.data[i].length, 0);
      for (j in this.data[i]) {
        svg.append("rect")
           .attr("x", (xpad/2)+(parseInt(j)+sx)*xscale)
           .attr("y", (ypad+10)+i*vertscale)
           .attr("width", xscale-2)
           .attr("height", vertscale-2)
           .attr("fill", "rgb("+parseInt(yscale*this.data[i][j])+",0,10)");
      } 
      

    }
  }

  /* Line-plot drawing */
  this.draw_line_plot=draw_line_plot;
  function draw_line_plot () {
    // Clear the palate
    svg.selectAll("rect").remove();
    svg.selectAll("line").remove();
    // Canvas (background)
    svg.append("rect")
       .attr("x", 0)
       .attr("y", 20)
       .attr("rx", 5)
       .attr("ry", 5)
       .attr("width", this.w-5)
       .attr("height", this.h-20)
       .attr("fill", this.bgcolor);

    // Update dataset values
    data_max_y = 0;
    for (var i in this.data) {
      this.data[i].splice(0,this.data[i].length-this.point_width);
      data_max_y = Math.max(Math.max.apply(Math, this.data[i]), data_max_y);
      data_max_x = Math.max(data_max_x, this.data[i].length);
    }
    data_max_y = Math.max(this.ymax, data_max_y);

    // Calculate scaling
    yscale = (this.h-ypad-ypad)/data_max_y;
    xscale = (this.w-xpad)/this.point_width;

    // Draw axes
    svg.append("line")
       .attr("x1", xpad)
       .attr("y1", 20)
       .attr("x2", xpad)
       .attr("y2", this.h)
       .style("stroke", "rgb(100,100,100)");
    svg.append("line")
       .attr("x1", 0)
       .attr("y1", this.h-ypad)
       .attr("x2", this.w-5)
       .attr("y2", this.h-ypad)
       .style("stroke", "rgb(100,100,100)");

    // Add lines
    for (i in this.data) {
      var sp = Math.max(this.data[i].length-this.point_width,0);
      //var sx = data_max_x-this.data[i].length;
      var sx = this.point_width-this.data[i].length;
      for (j=0; j<this.point_width-1; j++) {
        if (j==this.data[i].length-1) break;
        svg.append("line")
           .attr("x1", xpad+(j+sx)*xscale)
           .attr("y1", this.h-(ypad+this.data[i][sp+j]*yscale))
           .attr("x2", xpad+(j+1+sx)*xscale)
           .attr("y2", this.h-(ypad+this.data[i][sp+j+1]*yscale))
           .style("stroke", this.colors[i]);
      }
    }

    // x tick
    console.log("Tick scale:");
    num_div = 6;
    scale = parseInt(data_max_y/num_div);
    console.log(scale);

    //for (i=0; i<=6; i++) {
    //  var j = i*(data_max_y*1.1)/6;
    //  console.log(j)
    //}
   // console.log(data_max_y);
   // console.log([1,2,1,4,2,1]);

    // Label
    svg.append("text")
       .text(this.xlabel)
       .attr("x", this.w-20)
       .attr("y", this.h-(ypad/4))
       .attr("font-family", "sans-serif")
       .attr("font-size", ypad*.9)
       .attr("fill", "black")
       .attr("text-anchor", "end");
    svg.append("text")
       .text(this.ylabel)
       .attr("x", 20)
       .attr("y", 30)
       .attr("transform", "rotate(270 20 30)")
       .attr("font-family", "sans-serif")
       .attr("font-size", ypad*.9)
       .attr("fill", "black")
       .attr("text-anchor", "end");
  }

  /* This method is shared for all plots */
  this.redraw=redraw;
  function redraw () {
    svg.remove();
    svg = canvas.append("svg").attr("width", this.w).attr("height", this.h);
    svg.attr("width", this.w).attr("height", this.h);
    this.draw();
  }

}
