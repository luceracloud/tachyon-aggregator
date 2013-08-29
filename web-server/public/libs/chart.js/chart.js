/**
*example how to use
$(document).ready(function() {
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

    setTimeout(function() {
        //how to update the graph
        donut.update({
            //mandatory
            percentages : [40, 50, 10],
            sectorClasses : ['negative', 'neutral'],
            //optional
            mainText : '40%',
            secondaryText : 'new secondary text'
        });
    }, 1000);

    return false;
});
*/

/**
 * creates the object Donut with data values
 * @param data
 * @constructor
 */
function Donut(data) {
    //donut radius
    this.outerRadius = data.outerRadius;

    //the radius of the donut hole
    this.innerRadius = data.innerRadius;

    //size of the raphael canvas
    this.canvasSize = {
        width : data.canvasSize.width,
        height : data.canvasSize.height
    };

    //user interaction
    this.enableInteraction = data.enableInteraction || false;

    //class of each sector
    this.sectorClasses = data.sectorClasses;

    //if the graph has blank sectors
    if (data.hasBlankSectors) {
        this.blankSectorPercentage = 0.0025;
    } else {
        this.blankSectorPercentage = 0;
    }

    this.calculateSectorPercentages(data.percentages);

    //Non Mandatory attributes
    var elementForRaphael = data.htmlReceptorID == null ? data.receptorElement : data.htmlReceptorID;

    //raphael object where the donut will be draw
    this.raphael = Raphael(elementForRaphael, '100%', '100%');

    //in pratical terms a viewbox acts like a window for the svg when it is drawn.
    //we can draw a huge svg, but if it has a small viewbox, only the stuff that fits in the window/viewbox will be shown
    //in this case we want to show the whole svg we just draw
    this.raphael.setViewBox(0, 0, data.canvasSize.width, data.canvasSize.height);

    //hack related to an open bug on raphael: https://github.com/DmitryBaranovskiy/raphael/issues/649
    //this set the behaviour the svg should take when scaling the element
    //check http://www.w3.org/TR/SVG11/coords.html#PreserveAspectRatioAttribute for more info
    this.raphael.canvas.setAttribute('preserveAspectRatio', 'xMinYMin meet');

    //TEXT attributes
    this.mainText = data.mainText || '';
    this.mainTextClass = data.mainTextClass || 'donutMainText';
    this.secondaryText = data.secondaryText || '';
    this.secondaryTextClass = data.secondaryTextClass || 'donutSecondaryText';

    this.mainTextObj = null;
    this.secondaryTextObj = null;

    this.id = data.id || null;

    this.sectors = new Array();
}

/**
 * draws the donut
 * @param data
 */
Donut.prototype.create = function() {
    var donut = this;
    var previousSectorAnglesSum = 0;
    var secondEdgePoint, thirdEdgePoint, fourthEdgePoint, currentSectorAngle, currentSector, isFakeSector;

    var canvasCenter = {
        x : this.canvasSize.width/2.0,
        y : this.canvasSize.height/2.0
    };

    //the donut is created exactly in the middle of the canvas
    var firstEdgePoint = {
        x : canvasCenter.x,
        y : canvasCenter.y - this.outerRadius
    };

    //a donut is composed by a bunch of donut sectors
    for (var i = 0; i < this.sectorPercentages.length; i++) {
        isFakeSector = (this.blankSectorPercentage != 0 && i%2 != 0);

        //the angle in radians of the current sector
        currentSectorAngle = this.sectorPercentages[i] * 2 * Math.PI;

        //calculating the edge points of the sector
        secondEdgePoint = this.calculateEdgePointInDonut(currentSectorAngle + previousSectorAnglesSum, this.outerRadius);
        thirdEdgePoint = this.calculateEdgePointInDonut(currentSectorAngle + previousSectorAnglesSum, this.innerRadius);
        fourthEdgePoint = this.calculateEdgePointInDonut(previousSectorAnglesSum, this.innerRadius);

        //creates the donut sector
        currentSector = new DonutSector({
            //needs to be done, otherwise when showing data to the user the sector would have a little bit less because of the fake sectors
            percentage : this.sectorPercentages[i] + this.blankSectorPercentage,

            //we need this to ensure that fake sectors have no interaction possible
            isFakeSector : isFakeSector,
            startAngle : previousSectorAnglesSum,
            angle : currentSectorAngle,
            class : this.sectorClasses[i],
            externalRadius : this.outerRadius,
            internalRadius : this.innerRadius,
            vertices : [firstEdgePoint, secondEdgePoint, thirdEdgePoint, fourthEdgePoint]
        });

        //draws the sector
        currentSector.drawInRaphael(this.raphael);

        //keeps track of our sectors, including the fake ones because of the update function
        this.sectors.push(currentSector);

        if (this.enableInteraction && !currentSector.isFakeSector) {
            //raphaelShape events
            currentSector.raphaelShape.click(function() {
                donut.toggleHighlightForSector(this.sector);
            });
        }

        //the sum of all previous angles
        previousSectorAnglesSum += currentSectorAngle;

        //the next sector begins at the second edge point, the end point of the external arc
        firstEdgePoint = secondEdgePoint;
    }

    this.updateTexts();
}

/**
* updates the texts acording to the donut text properties
*/
Donut.prototype.updateTexts = function() {
    this.mainTextObj != null ? this.mainTextObj.remove() : null;
    this.secondaryTextObj != null ? this.secondaryTextObj.remove() : null;

    var canvasCenter = {
        x : this.canvasSize.width/2.0,
        y : this.canvasSize.height/2.0
    };

    //main text
    this.mainTextObj = this.raphael.text(canvasCenter.x, canvasCenter.y, this.mainText);

    //class to style it with css
    this.mainTextObj.node.className.baseVal += ' ' + this.mainTextClass;

    //secondary text
    this.secondaryTextObj = this.raphael.text(canvasCenter.x, canvasCenter.y, this.secondaryText);

    //class to style it with css
    this.secondaryTextObj.node.className.baseVal += ' ' + this.secondaryTextClass;    

    //hack to position the texts inside the circle
    var mainTextHeight = this.mainTextObj.getBBox().height;
    var secondaryTextHeight = this.secondaryTextObj.getBBox().height;

    this.mainTextObj.attr({
        x : canvasCenter.x,
        y : canvasCenter.y - secondaryTextHeight * 0.5
    });

    this.secondaryTextObj.attr({
        x : canvasCenter.x,
        y : canvasCenter.y + mainTextHeight * 0.5
    });
}

/**
 * updates the current donut to match the new values (angle, classes)
 * @param data
 */
Donut.prototype.update = function(data) {
    var sector;

    //hide and delete from the dom all the sectors
    for (var i = 0; i < this.sectors.length; i++) {
        sector = this.sectors[i];

        //makes the sector invisible and then removes it
        sector.raphaelShape.animate({opacity : 0.0}, 100, 'linear', function() {this.remove();});
    }

    //get rid of objects that don't exist anymore
    this.sectors = new Array();

    //class of each sector
    this.sectorClasses = data.sectorClasses;

    //recalculate the percentages for each sector
    this.calculateSectorPercentages(data.percentages);

    //text update
    this.mainText = (data.mainText !== null ? data.mainText : this.mainText);
    this.secondaryText = (data.secondaryText !== null ? data.secondaryText : this.secondaryText);

    //check if selector has changed
    this.mainTextClass = (data.mainTextClass !== null ? data.mainTextClass : this.mainTextClass);
    this.secondaryTextClass = (data.secondaryTextClass !== null ? data.secondaryTextClass : this.secondaryTextClass);

    //creates the donut again
    var donut = this;
    donut.create();
}

/**
* donut is the delegate of the highlighting of sector to make sure that in maximum only one sector is highlighted at a time
* @param sector to highlight/unhighlight
*/
Donut.prototype.toggleHighlightForSector = function(sector) {
    //if one sector is highlighted we can just unhighlight it and we don't need to check the rest of the sectors
    if (sector.isHighlighted) {
        sector.toggleHighlight();
    } else {
        var currentSector;

        //unhighlights any highlighted sector if there is one
        for (var i = 0; i < this.sectors.length; i++) {
            currentSector = this.sectors[i];

            if (currentSector.isHighlighted) {
                currentSector.toggleHighlight();

                //only one sector is highlighted at a time
                break;
            }
        }
        sector.toggleHighlight();
    }
}

/**
 * calculates the normalized sector percentages [0,1] from the percentages [0,100] sent and adds blank sectors if needed
 */
Donut.prototype.calculateSectorPercentages = function(nonNormalizedPercentages) {
    //sector percentages
    this.sectorPercentages = new Array();

    //calculates the sector angles and the normalized percentages [0, 1] of each sector
    for (var i = 0; i < nonNormalizedPercentages.length; i++) {
        this.sectorPercentages.push(nonNormalizedPercentages[i] / 100.0 - this.blankSectorPercentage);

        //we add a fake blank sector per real sector to fake the gaps
        if (this.blankSectorPercentage > 0.0) {
            this.sectorPercentages.push(this.blankSectorPercentage);

            //adds the class for the fake sector
            this.sectorClasses.splice(i*2 + 1, 0, 'graph-sector-invisible');
        }
    }

    //TODO fix this hack, because it doesn't work with [100] (pie charts with only one sector)
    if (this.sectorPercentages.length == 1) {
        this.sectorPercentages = [99.9999];
    }
}

/**
 * calculates a (inner/outer) edge point at angle "radians" for the donut
 * @param radians, angle in radians
 * @param radius, it can be an inner edge point or an outside edge point
 * @returns point
 */
Donut.prototype.calculateEdgePointInDonut = function(radians, radius) {
    return {
        //we add PI/2.0 because we begin to draw at PI/2.0 instead of 0 radians like in the trygonometrical circle
        //we also add this.canvasSize/2.0 to switch the origin of the referencial (this.canvasSize.width/2.0, this.canvasSize.height/2.0)
        x : Math.cos(radians + (Math.PI / 2.0)) * radius + this.canvasSize.width * 0.5,

        //we add 3*PI/2.0 because we begin to draw at PI/2.0 instead of 0 radians like in the trygonometrical circle
            //we also add this.canvasSize/2.0 to switch the origin of the referencial, our referencial is at the centre of the circle (this.canvasSize.width/2.0, this.canvasSize.height/2.0)
        y : Math.sin(radians + (3.0 * Math.PI / 2.0)) * radius + this.canvasSize.height * 0.5
    };
}



//DONUT SECTOR

/**
 * creates the object DonutSector with data values
 * @param data
 * @constructor
 */
function DonutSector(data) {
    //fake sectors have no interaction, they are just used for visual porposes
    this.isFakeSector = data.isFakeSector || false;

    //percentage of the sector [0,100]
    this.percentage = data.percentage;

    //the arc angle of the sector
    this.angle = data.angle;

    //the angle where the sector starts at the circle
    this.startAngle = data.startAngle;

    //class of the sector
    this.class = data.class;

    //vertices to build the svg path
    this.vertices = data.vertices;

    //to use in the arcs in conjunction with the angle
    this.externalRadius = data.externalRadius;
    this.internalRadius = data.internalRadius;

    //the raphael svg object
    this.raphaelShape = null;

    this.isHighlighted = false;
}

/**
 * draws the sector in inside the raphael svg conatainer
 * @param raphael
 */
DonutSector.prototype.drawInRaphael = function(raphael) {
    this.raphaelShape = raphael.path(
        'M' + this.vertices[0].x + ',' + this.vertices[0].y +
        ' A' + this.externalRadius + ',' + this.externalRadius + ' 0 ' + this.isLargeArc(this.angle) + ' 0 ' +  this.vertices[1].x + ',' + this.vertices[1].y +
        ' L' + this.vertices[2].x + ',' + this.vertices[2].y +
        ' A' + this.internalRadius + ',' + this.internalRadius + ' 0 ' + this.isLargeArc(this.angle) + ' 1 ' +  this.vertices[3].x + ',' + this.vertices[3].y + 'z');

    //arc attributes
    this.raphaelShape.node.className.baseVal += ' ' + this.class;

    //because of the click event is being attached inside the for loop in the donut
    this.raphaelShape.sector = this;
}

/**
 * toggles the highlight status of the graph
 */
DonutSector.prototype.toggleHighlight = function(radians) {
    var movingAngle = this.startAngle + this.angle*0.5;
    var distanceToMove = 40.0;
    var duration = 100;

    if (this.isHighlighted) {
        this.raphaelShape.animate({
            transform : this.raphaelShape.transform().toString() + ' t' + -Math.cos(movingAngle + (Math.PI / 2.0)) * distanceToMove + ',' + -Math.sin(movingAngle + (3.0 * Math.PI / 2.0)) * distanceToMove
        }, duration);
    } else {
        this.raphaelShape.animate({
            transform : this.raphaelShape.transform().toString() + ' t' + Math.cos(movingAngle + (Math.PI / 2.0)) * distanceToMove + ',' + Math.sin(movingAngle + (3.0 * Math.PI / 2.0)) * distanceToMove
        }, duration);
    }

    this.isHighlighted = !this.isHighlighted;
}

/**
 * determines if the arc is the large one or the small one, look at the svg documentation about arcs
 * http://www.w3.org/TR/SVG/paths.html#PathDataEllipticalArcCommands
 * @param raphael
 * @return string 1 or 0
 */
DonutSector.prototype.isLargeArc = function(radians) {
    /* Of the four candidate arc sweeps, two will represent an arc sweep of greater than or equal to 180 degrees (the "large-arc"),
    and two will represent an arc sweep of less than or equal to 180 degrees (the "small-arc"). If large-arc-flag is '1',
    then one of the two larger arc sweeps will be chosen; otherwise, if large-arc-flag is '0', one of the smaller arc sweeps will be chosen, */
    return radians >= Math.PI ? ' 1 ' : ' 0 ';
}

