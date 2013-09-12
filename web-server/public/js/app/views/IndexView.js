// IndexView.js

define(["jquery", "backbone", "events/Notifier", "socketio", "views/PlotView", "text!templates/Index.html"],

    function($, Backbone, Notifier, io, PlotView, template) {

        var IndexView = Backbone.View.extend({

            // The DOM Element associated with this view
            el: ".magic",
            i: 0,
            charts: [],
            socket: io.connect('http://localhost:1337'),
            zone: "null",
            plot_id: 0,

            // View constructor
            initialize: function() {

                // Calls the view's render method
                this.render();
                this.connect();
                this.windowResize();
                
                $(window).on('resize', this.resized, this);

                Notifier.on('getZones', this.getZones, this);
                Notifier.on('zoomIn', this.zoomIn, this);
                Notifier.on('zoomOut', this.zoomOut, this);
                Notifier.on('changeStyle', this.changeStyle, this);
                Notifier.on('deletePlot', this.deletePlot, this);
                Notifier.on('modalAdd', this.modalAdd, this);
                Notifier.on('requestInstance', this.requestInstance, this);
                Notifier.on('changeConfig', this.changeConfig, this);
                Notifier.on('changeId', this.changeId, this);
                Notifier.on('removeStat', this.removeStat, this);
            },

            // Sends server a message letting it know to stop sending certain stat
            // and removes the line from the associated chart-d3 object
            removeStat: function(e) {
                var info = e.target.name.split(',');
                this.charts[this.plot_id].removeDataKey(info);

                this.socket.emit('message', {
                    'type': 'cancel',
                    'plt': this.plot_id,
                    'info': info
                });
            },

            // A way to keep track of what plot is currently being modified; to deal with weird name/selection
            // conventions in the view rending service.
            changeId: function(id) {
                this.plot_id = id;
            },

            // Script to run when user clicks on "Config..." button, populate with approrpiate stats
            changeConfig: function(id) {
                var foreText = '<table class="table table-striped well"><tr><td>';
                var postTextA = '</td><td><button class="btn btn-xs btn-danger pull-right" name="'
                var postTextB = '" id="remove-stat-btn"><d class="glyphicon glyphicon-trash"></d>' +
                                    ' Remove Stat</button></td></tr>';
                var stats = this.charts[id].keys();               
                var bodyText = "";
                for (var s in stats) {
                    bodyText += foreText + stats[s][0] + ":" + stats[s][1] + ":" + stats[s][2] + postTextA +
                                stats[s][0] + "," + stats[s][1] + "," + stats[s][2] + postTextB;
                }
                var statsField = document.getElementById("config-stats-" + this.plot_id);
                statsField.innerHTML = bodyText;
            },

            windowResize: function(){
                //$.each(this.charts, function(i, chart){
                //    chart.w = $('.chart').width();
                //});
            },

            // View Event Handlers
            events: {
                'click #add-plot': 'getPlotName'
            },

            getZones: function() {
              this.socket.emit('message', {
                'type': 'init', 'self': 'global'
              });
            },

            connect: function() {
                var _this = this;
                this.socket.on('message', function(data) {
                    if (data.type == 'machine_list') {
                        _this.populateFields(data.list);
                    }
                    if (data.type == 'value-response') {
                        _this.addDataPoint(data);
                    }
                    if (data.type == 'instance-response') {
                        _this.populateInstances(data);
                    }

                });
                this.socket.emit('message', {
                    'type': 'init',
                    'self': 'global'
                });
            },

            // Add instance names returned from server to appropriate 'select' box
            populateInstances: function(data) {
      console.log(data);
                var instance_select_box=document.getElementById("instance_select_cpu_" + data.self);
                instance_select_box.options.length = 1;
                for (var d in data.data) {
                    instance_select_box.options[parseInt(d)+1]=new Option(data.data[d], data.data[d]);
                }
                var instance_select_box=document.getElementById("instance_select_network_" + data.self);
                instance_select_box.options.length = 1;
                for (var d in data.data) {
                    instance_select_box.options[parseInt(d)+1]=new Option(data.data[d], data.data[d]);
                }
                var instance_select_box=document.getElementById("instance_select_disk_" + data.self);
                instance_select_box.options.length = 1;
                for (var d in data.data) {
                    instance_select_box.options[parseInt(d)+1]=new Option(data.data[d], data.data[d]);
                }
            },

            // Request 'instance' information for a certain stat/zone
            requestInstance: function(data, evt) {
                var reqData = {};
                this.zone = evt.value;
                reqData['type'] = 'instance-req';
                reqData['info'] = [evt.value,'0',data];
                if (evt.id=="machine_select_cpu") {
                    reqData['info'][1] = 'cpu';
                } else if (evt.id=="machine_select_network") {
                    reqData['info'][1] = 'network';
                } else if (evt.id=="machine_select_disk") {
                    reqData['info'][1] = 'disk';
                } else {
                    return;
                }
                this.socket.emit('message', reqData);
            },

            // Take data as sent by server and add it to appropriate plot
            addDataPoint: function(data) {
                var chart = this.charts[data.plot];
                chart.addData(data["key"][0] +","+ data["key"][1] +","+ data["key"][2], data["value"]);
                chart.redraw();
            },

            // Adds GZ names to the associated 'select' boxes
            populateFields: function(data) {
                // cpu
                var machine_select_box=document.getElementById("machine_select_cpu");
                machine_select_box.options.length = 1;
                for (var id in data) {
                    machine_select_box.options[parseInt(id)+1]=new Option(data[id], data[id]);
                }
                // memory
                var machine_select_box=document.getElementById("machine_select_memory");
                machine_select_box.options.length = 1;
                for (var id in data) {
                    machine_select_box.options[parseInt(id)+1]=new Option(data[id], data[id]);
                }
                // network
                var machine_select_box=document.getElementById("machine_select_network");
                machine_select_box.options.length = 1;
                for (var id in data) {
                    machine_select_box.options[parseInt(id)+1]=new Option(data[id], data[id]);
                }
                // disk
                var machine_select_box=document.getElementById("machine_select_disk");
                machine_select_box.options.length = 1;
                for (var id in data) {
                    machine_select_box.options[parseInt(id)+1]=new Option(data[id], data[id]);
                }
                // other
                var machine_select_box=document.getElementById("machine_select_other");
                machine_select_box.options.length = 1;
                for (var id in data) {
                    machine_select_box.options[parseInt(id)+1]=new Option(data[id], data[id]);
                }
            },

            // Return the name of the associated plot
            getPlotName: function() {
                var name = $('#plot-name').val();
                this.addPlot(name);
                $('#plot-name').val('');
            },

            // Add a new plot to the page
            addPlot: function(name) {
                var html = new PlotView({model: {id: this.i, name: name}}).render().el;
                $(html).prependTo('#plots');
                var ele = d3.select('#chart-' + this.i);
                var chart = new chartd3(ele, 600, 220);
                this.charts.push(chart);
                this.i++;
                this.socket.emit('message', {
                    'type': 'init',
                    'self': 'global'
                });
            },

            // Zoom in the scale of the chart
            zoomIn: function(id) {
                var chart = this.charts[id];
                chart.point_width = parseInt(chart.point_width / 1.1, 10) - 1;
            },

            // Zoom out the scale of the chart
            zoomOut: function(id) {
                var chart = this.charts[id];
                chart.point_width = parseInt(chart.point_width * 1.1, 10) + 1;
            },

            // Change between line plot and heatmap
            changeStyle: function(id, e) {
                var chart = this.charts[id];
                if (e.target.id=='style-line') {
                    chart.modell='line';
                } else if (e.target.id=='style-heatmap') {
                    chart.modell='heatmap';
                }
            },

            // Add a new statistic to the associated plot
            modalAdd: function(id, type) {
                var s_type = document.getElementById("stat_select_"+type+"_"+id);
                var i_selected = "null";
                if (type=="memory" || type=="other") {
                } else {
                    var i_type = document.getElementById("instance_select_"+type+"_"+id);
                    i_selected = i_type.options[i_type.selectedIndex].value;
                }
                this.socket.emit('message', {
                    'type': 'request',
                    'self': this.plot_id,
                    'info': [type,
                            this.zone,
                            s_type.options[s_type.selectedIndex].value,
                            i_selected]
                });
            },

            // Cancels the server's sending statistics
            deletePlot: function(id) {
                var chart = this.charts[id];
                chartStats = chart.keys();
                for (stat in chartStats) {
                    this.socket.emit('message', {
                        'type': 'cancel',
                        'plt': id,
                        'info': chartStats[stat]
                    });
                }
            },

            // Renders the view's template to the UI
            render: function() {

                // Setting the view's template property using the Underscore template method
                this.template = _.template(template, {});

                // Dynamically updates the UI with the view's template
                this.$el.html(this.template);

                this.addPlot('Plot 1');

                // Maintains chainability
                return this;

            }

        });

        // Returns the View class
        return IndexView;

    }

);
