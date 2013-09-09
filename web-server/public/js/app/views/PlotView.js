// IndexView.js

define(["jquery", "backbone", "events/Notifier", "text!templates/Plot.html"],

    function($, Backbone, Notifier, template){

        var PlotView = Backbone.View.extend({

            // The DOM Element associated with this view
            tagName: "div",

            // View constructor
            initialize: function() {

                // Calls the view's render method
                this.render();
            },

            // View Event Handlers
            events: {
                'click #delete-plot': 'deletePlot',
                'click #zoom-in': 'zoomIn',
                'click #zoom-out': 'zoomOut',
                'click #style-line': 'changeStyle',
                'click #style-heatmap': 'changeStyle',
                'click #modal-add-cpu': 'modalAdd',
                'click #modal-add-memory': 'modalAdd',
                'click #modal-add-network': 'modalAdd',
                'click #modal-add-disk': 'modalAdd',
                'click #modal-add-other': 'modalAdd',
                'click #change-config': 'changeId',
                'click #change-config': 'changeConfig',
                'click #add-metric-btn': 'changeId',
                'click #remove-stat-btn': 'removeStat',
                'change #machine_select_cpu': 'requestInstance',
                'change #machine_select_memory': 'requestInstance',
                'change #machine_select_network': 'requestInstance',
                'change #machine_select_disk': 'requestInstance',
                'change #machine_select_other': 'requestInstance',
            },

            removeStat: function(e) {
                e.preventDefault();
                Notifier.trigger('removeStat', e);
            },

            changeConfig: function(e) {
                e.preventDefault();
                console.log(e);
                Notifier.trigger('changeId', this.model.id);
                Notifier.trigger('changeConfig', this.model.id);
            },

            changeId: function(e) {
                e.preventDefault();
                console.log(this.model.id);
                console.log("INNER CHANGING ID TO ");
                console.log(e);
                Notifier.trigger('changeId', this.model.id);
            },

            modalAdd: function(e) {
                e.preventDefault();
                var type;
                if (e.target.id=="modal-add-cpu") {
                    type = "cpu";
                } else if (e.target.id=="modal-add-memory") {
                    type = "memory";
                } else if (e.target.id=="modal-add-network") {
                    type = "network";
                } else if (e.target.id=="modal-add-disk") {
                    type = "disk";
                } else if (e.target.id=="modal-add-other") {
                    type = "other";
                } else {}
                console.log(this.model.id);
                //console.log(this.modal.id);
                Notifier.trigger('modalAdd', e.target.name, type);
            },

            zoomIn: function(e) {
                e.preventDefault();
                console.log(this.model.id);
                Notifier.trigger('zoomIn', this.model.id);
            },

            zoomOut: function(e) {
                e.preventDefault();
                Notifier.trigger('zoomOut', this.model.id);
            },

            changeStyle: function(e) {
                e.preventDefault();
                Notifier.trigger('changeStyle', this.model.id, e);
            },

            deletePlot: function(e) {
                e.preventDefault();
                Notifier.trigger('deletePlot', this.model.id);
                this.remove();
            },

            requestInstance: function(e) {
                Notifier.trigger('requestInstance', this.model.id, e.target);
            },

            // Renders the view's template to the UI
            render: function() {

                // Setting the view's template property using the Underscore template method
                this.template = _.template(template, this.model);

                // Dynamically updates the UI with the view's template
                this.$el.html(this.template);

                // Maintains chainability
                return this;

            }

        });

        // Returns the View class
        return PlotView;

    }

);
