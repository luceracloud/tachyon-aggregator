//<script type="text/javascript">

/* Formatting numbers */
var w = 500;
var h = 100;

/* Some nice random data */
var dataset = [];
for (var i = 0; i < 20; i++) {
	var newNumber = Math.random() * 30 + 5;
	dataset.push(newNumber);
}

/* Make svg zone */
var svg = d3.select("body")
			.append("svg")
			.attr("width", w)
			.attr("height", h);

/* Make bars */
svg.selectAll("rect")
   .data(dataset)
   .enter()
   .append("rect")
   .attr("x", function(d,i) { return(i*(w/dataset.length)); } )
   .attr("y", function(d) { return d; } )
   .attr("width", (w/dataset.length)-1)
   .attr("height", 100)
   .attr("fill", function(d) { return "rgba(25,10,200,1.0)"; });

/* Make labels */
svg.selectAll("text")
   .data(dataset)
   .enter()
   .append("text")
   .text(function(d) { return Math.floor(d) ; })
   .attr("x", function(d,i) { return(i*(w/dataset.length)+(w/dataset.length)/2); } )
   .attr("y", function(d) { return d+16; } )
   .attr("font-family", "sans-serif")
   .attr("font-size", "11px")
   .attr("fill", "white")
   .attr("text-anchor", "middle");


//</script>