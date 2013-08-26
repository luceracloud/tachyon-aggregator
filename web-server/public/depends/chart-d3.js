
// We assume d3 has already been loaded

function chartd3 (canvas, w, h) {

  var svg;
  svg = canvas.append("svg").attr("width", w).attr("height", h);

  this.existance=1;
  this.update=true;
  this.delay = 1000; // in ms
  this.w = w;
  this.h = h;
  this.colors = [ "blue", "red", "green", "orange" ];
  this.point_width = 10;
  this.scale=true; // If true, we rescale the y-scale each update
  this.bgcolor="rgb(240,240,240)";

  this.ymax = 0;


  /* Private vars */
  yscale = 1;
  xscale = 1;
  ypad = 20;
  xpad = 40;

  // Init drawing
  svg.append("rect")
     .attr("x", 0)
     .attr("y", 0)
     .attr("rx", 5)
     .attr("ry", 5)
     .attr("width", w-5)
     .attr("height", h)
     .attr("fill", this.bgcolor);



  data = {};
  var data_max_y = 0;
  var data_max_x = 0;

  // Public functions
  this.funcOne=funcOne;
  function funcOne () {
    console.log("test");

  }

  this.addData=addData;
  function addData (key, new_data) {
    data[Object.keys(data).length] = new_data;
    this.draw;
  }

  this.draw=draw;
  function draw() {
    // Clear the palate
    data_max_y = 0;
    svg.selectAll("line").remove();

    // Update datasets
    for (var i in data) {
      data[i].splice(0,data[i].length-this.point_width);
      data_max_y = Math.max(Math.max.apply(Math, data[i]), data_max_y);
      data_max_x = Math.max(data_max_x, data[i].length);
    }
    data_max_y = Math.max(this.ymax, data_max_y);

    // Calculate scaling
    yscale = (this.h-ypad-ypad)/data_max_y;
    xscale = (this.w-xpad)/this.point_width;

    // Draw axes
    svg.append("line")
       .attr("x1", xpad)
       .attr("y1", 0)
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
    for (i in data) {
      var sp = Math.max(data[i].length-this.point_width,0);
      var sx = data_max_x-data[i].length;
      for (j=0; j<this.point_width-1; j++) {
        if (j==data[i].length-1) break;
        svg.append("line")
           .attr("x1", xpad+(j+sx)*xscale)
           .attr("y1", this.h-(ypad+data[i][sp+j]*yscale))
           .attr("x2", xpad+(j+1+sx)*xscale)
           .attr("y2", this.h-(ypad+data[i][sp+j+1]*yscale))
           .style("stroke", this.colors[i]);
      }
    }

  }

  // Redraw the data
  this.redraw=redraw;
  function redraw () {
    svg.remove();
    svg = canvas.append("svg").attr("width", this.w).attr("height", this.h);
    svg.attr("width", this.w).attr("height", this.h);
    svg.selectAll("rect").remove();

    svg.append("rect")
       .attr("x", 0)
       .attr("y", 0)
       .attr("rx", 5)
       .attr("ry", 5)
       .attr("width", this.w-5)
       .attr("height", this.h)
       .attr("fill", this.bgcolor);
    this.draw();
  }

  //
  // Takes an input column vector and appends data to array.
  //
  this.addColumn=addColumn;
  function addColumn (new_data) {
    if (new_data.length>Object.keys(data).length) {
      for (var i=0; i<new_data.length-Object.keys(data).length; i++) {
        data[Object.keys(data).length] = [];
      }
    }
    for (var i in new_data) {
      data[i].push(new_data[i]);
    }
    
    data_max_y = Math.max(Math.max.apply(Math, new_data), data_max_y);
    this.draw();
  }

  // Clears the data, but keeps the position in memory
  this.clearData=clearData;
  function clearData (group) {
    toClear = [];
    if (typeof(group) == "undefined") {
      for (var set in Object.keys(data)) {
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
      data[k].length = 0;
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

  /*
  if (this.update) {
    setInterval(function() {
      this.draw;
    }, this.delay);
  }
  */





}