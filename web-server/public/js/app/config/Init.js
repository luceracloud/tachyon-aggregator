// Init.js
// =======

require.config({

  // Sets the js folder as the base directory for all future relative paths
  baseUrl: "./js",

  // 3rd party script alias names (Easier to type "jquery" than "libs/jquery, etc")
  // probably a good idea to keep version numbers in the file names for updates checking
  paths: {

      // Core Libraries
      // ==============

      "jquery": "libs/jquery/jquery",

      "underscore": "libs/lodash/lodash",

      "backbone": "libs/backbone/backbone",

      "bootstrap": "libs/bootstrap/dist/js/bootstrap",

      // Plugins
      // =======

      "backbone.validateAll": "libs/plugins/Backbone.validateAll",

      "text": "libs/text/text",

      "socketio": "../socket.io/socket.io",

      "d3": "libs/d3/d3",

      "chart-d3": "libs/plugins/chart-d3",

      // Application Folders
      // ===================

      "collections": "app/collections",

      "models": "app/models",

      "routers": "app/routers",

      "templates": "app/templates",

      "views": "app/views",

      "events": "app/events"

  },

  // Sets the configuration for your third party scripts that are not AMD compatible
  shim: {

      // Bootstrap
      "bootstrap": ["jquery"],

      "d3": {

        "exports": "d3"

      },

      // Chart-D3
      "chart-d3": ["d3"],

      // Socket IO
      "socketio": {

        "exports": "io"

      },

      // Backbone
      "backbone": {

        // Depends on underscore/lodash and jQuery
        "deps": ["underscore", "jquery"],

        // Exports the global window.Backbone object
        "exports": "Backbone"

      },

      // Backbone.validateAll plugin that depends on Backbone
      "backbone.validateAll": ["backbone"]

  }

});

// Includes Desktop Specific JavaScript files here (or inside of your Desktop router)
require(["jquery", "backbone", "routers/Router", "backbone.validateAll", "bootstrap", "d3", "chart-d3"],

  function($, Backbone, Router, validate, bootstrap, d3, chartd3) {

    // Instantiates a new Desktop Router instance
    new Router();

  }

);
