// DEPENDENCIES
// ============

var Config =  global.Config = require('./config/config.js').config;
    express = require("express"),
    http =    require("http"),
    port =    ( process.env.PORT || Config.listenPort ),
    server =  module.exports = express(),
    app =     http.createServer(server),
    io =      require('socket.io').listen(app),
    API =     require('./API');

// SERVER CONFIGURATION
// ====================

server.configure(function() {

  server.use(express["static"](__dirname + "/../public"));

  server.use(express.errorHandler({

    dumpExceptions: true,

    showStack: true

  }));

  server.use(express.bodyParser());

  server.use(server.router);

});

// API
// ===

API.api(server, io);

// Start Node.js Server
app.listen(port);

console.log('\n\nWelcome to The Stack!\n\nPlease go to http://localhost:' + port + ' to start using Require.js and Backbone.js\n\n');
