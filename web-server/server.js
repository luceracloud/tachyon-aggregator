/*
 *
 *
 *  server.js
 *
 *
 *
 *
 *  CREATED:  23 AUG 2013
 *  EDITED:   23 AUG 2013
 */

var express = require('express');

var app = express();
var http = require('http');
var server = http.createServer(app);
var io = require('socket.io').listen(server);

app.configure(function() {
  app.use(express.static(__dirname + '/public'));
});

server.listen(80);

