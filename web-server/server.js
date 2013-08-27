/*
 *  server.js
 *
 *
 *
 *
 *  CREATED:  23 AUG 2013
 *  EDITED:   26 AUG 2013
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

server.listen(1337);

var machine_list = [];


/* Handling zmq/protobuf parsing */
var zmq = require('zmq');
var sock = zmq.socket('sub');
var ProtoBuf = require('protobufjs');
var Packet = ProtoBuf.protoFromFile("pckt.proto").build("PBMSG.Packet");

sock.subscribe("");
sock.connect('tcp://172.20.0.85:7211');



/* Handling http requests */
io.sockets.on('connection', function(socket) { 

  STAT_ARRAY = [];

  /* Start listening to zmq/protobuf */
  sock.on("message", function(msg) {
    var message;
    try {
      message = Packet.decode(msg);
    } catch (err) {
      return;
    }

    for (s in STAT_ARRAY) {
      if (message.name==STAT_ARRAY[s][1][1]) {
        // add data to send buffer
        var dataToSend = {};
        dataToSend["type"] = "value-response";
        dataToSend["plot"] = STAT_ARRAY[s][0];
        dataToSend["key"] = [STAT_ARRAY[s][1][1],
                            STAT_ARRAY[s][1][2],
                            STAT_ARRAY[s][1][3]]
        // Depending on cpu vs. std vs. else, deal with
        // instance name differently
       // console.log(STAT_ARRAY[stat][1]);
        if (STAT_ARRAY[s][1][0]=="cpu") {
          console.log("cpu intercept");
        } else if (STAT_ARRAY[s][1][0]=="memory") {
          console.log(message);
          if (STAT_ARRAY[s][1][2]=="rss") {
            dataToSend["value"] = message.mem[0].rss;
          } else if (STAT_ARRAY[s][1][2]=="swap") {
            dataToSend["value"] = message.mem[0].swap;
          } else if (STAT_ARRAY[s][1][2]=="major faults") {
            dataToSend["value"] = message.mem[0].maj_fault;
          } else if (STAT_ARRAY[s][1][2]=="minor faults") {
            dataToSend["value"] = message.mem[0].as_fault;
          } else if (STAT_ARRAY[s][1][2]=="page-ins") {
            dataToSend["value"] = message.mem[0].pgin;
          } else {
            console.log("Unknown 'memory' statistic");
          }
        } else if (STAT_ARRAY[s][1][0]=="network") {
          console.log("network intcpt");
        } else if (STAT_ARRAY[s][1][0]=="disk") {
          console.log("disk inctp");
        } else if (STAT_ARRAY[s][1][0]=="other") {
          if (STAT_ARRAY[s][1][2]=="number of processes") {
            dataToSend["value"] = message.processes;
          } else if (STAT_ARRAY[s][1][2]=="number of threads") {
            dataToSend["value"] = message.threads;
          } else {
            console.log("Unknown 'other' statistic");
          }
        } else {
          console.log("Unknown request type. Ignoring");
        }
        if (dataToSend["value"]==null) dataToSend["value"]=0;
        socket.emit("message", dataToSend);
      }
    }

    if (machine_list.indexOf(message.name)==-1) {
      machine_list.push(message.name);
    }
  });


  /* Send message with list of available machines
  socket.emit("message", {"type" : "cmd", "cmd" : "dfUse"});

  console.log("Connection");

  /* Handle new message events */
  socket.on( 'message', function(message, flags) {

    // Init / ident message
    if (message["type"]=="init") {
      console.log(machine_list);
      socket.emit("message", {"type" : "machine_list", "list" : machine_list})
    } else if (message["type"]=="request") {
      console.log(message);
      console.log(message["type"]);
      console.log(message["self"]);
      console.log(message["info"]);
      STAT_ARRAY.push([message["self"], message["info"]]); // { message["self"] : message["info"] } ); 
    }


  });


});
