/*
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
io.set('log level', 1);

server.listen(8000);




/* Handling zmq/protobuf parsing */
var machine_list = [];
machine_list.push("mac I");
machine_list.push("macc II");
machine_list.push("maccc III");
console.log(machine_list);


/* Handling http requests */
io.sockets.on('connection', function(socket) { 

  /* Send message with list of available machines
  socket.emit("message", {"type" : "cmd", "cmd" : "dfUse"});

  console.log("Connection");

  /* Handle new message events */
  socket.on( 'message', function(message, flags) {

    // Init / ident message
    if (message["type"]=="init") {
      console.log(machine_list);
      socket.emit("message", {"type" : "machine_list", "list" : machine_list})
    } else if (message["type"]="request") {
      console.log(message);

    }


  });


});
