chart.js
========

# Getting Started
The chart.js library is a graph library that enables you to create svg donut charts on your webpage with just a couple of lines of code. 

## Installation
The chart.js is a Bower package (which is in turn a [Node.js](http://nodejs.org/) package - you'll need that first), so you just need to install Bower -

    npm install bower -g

Having done that, your next step is

    bower install git@github.com:centralway/chart.js.git

Next, reference the Chart.js source file

    <script type="text/javascript" src="../dist/chart.js"></script>

## Usage
Create a div in your html page with an id of your taste, on this particular case "donutChart". 
Paste the next snippet of javascript code:

```javascript
//creates the graph but doesn't displays it
var donut = new Donut({
    innerRadius : 120,
    outerRadius : 180,
    canvasSize : {
        width : 500,
        height : 500 },
    percentages : [75, 25],
    sectorClasses : ['negative', 'neutral'],
    mainText : 'Main',
    secondaryText : 'Secondary',

    //not mandatory, default values assigned
    enableInteraction : true,
    htmlReceptorID : 'donutChart',
    mainTextClass : 'donutMainText',
    secondaryTextClass : 'donutSecondaryText',
    hasBlankSectors : true,
    id : 'ID1'
});

//display the graph
donut.create();
```

And voilà! You have your graph. You can style the graph with css, using the classes you defined upon creation ("mainTextClass", "seocndaryTextClass", "sectorClasses")

## Dependencies
The library depends on [raphaeljs](http://raphaeljs.com/) library which is "a small JavaScript library that should simplify your work with vector graphics on the web." 

#### Example
The repository contains a very simple example on how to use the library, is just a simple html page with a script that creates a donut graph.

### Unit tests
Incoming!

# Credits
Copyright (c) 2013 by Centralway Factory AG.

# Licensing Options
The Chart.js is licensed under free commercial and open source licenses for
application development, and a paid commercial license for OEM uses.

See LICENSE.txt for license.
