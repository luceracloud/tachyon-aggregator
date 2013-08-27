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
var Long = require('long'); 
var Packet = ProtoBuf.protoFromFile("pckt.proto").build("PBMSG.Packet");

sock.subscribe("");
sock.connect('tcp://172.20.0.85:7211');



/* Handling http requests */
io.sockets.on('connection', function(socket) { 

  var instances = {}; // key{zone; val{[cpu,net,disk]
  STAT_ARRAY = [];
  saved_msgs = {}; // key{name; val{msg

  /* Start listening to zmq/protobuf */
  sock.on("message", function(msg) {
    var message;
    try {
      message = Packet.decode(msg);
      //console.log(message);
    } catch (err) {
      return;
    }

    // Write packet to history, if necessary
    if (Object.keys(saved_msgs).indexOf(message.name)==-1) {
      saved_msgs[message.name]=message;
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
          dataToSend["value"] = 0;
          for (var i in message.cpu) {
            if (message.cpu[i].core==STAT_ARRAY[s][1][3]) {
              dataToSend["value"] = message.cpu[i].usage;
              break;
            }
          }
        } else if (STAT_ARRAY[s][1][0]=="memory") {
          if (STAT_ARRAY[s][1][2]=="rss") {
            dataToSend["value"] = new Long(message.mem[0].rss).toNumber();
          } else if (STAT_ARRAY[s][1][2]=="swap") {
            dataToSend["value"] = new Long(message.mem[0].swap).toNumber();
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
          dataToSend["value"] = 0;
          for (var i in message.net) {
            dataToSend["value"] = 0;
            if (message.net[i].instance==STAT_ARRAY[s][1][3]) {
              if (STAT_ARRAY[s][1][2]=="output bytes") {
              dataToSend["value"] = new Long(message.net[i].obytes64).toNumber();

              } else if (STAT_ARRAY[s][1][2]=="received bytes") {
              dataToSend["value"] = new Long(message.net[i].rbytes64).toNumber();

              } else if (STAT_ARRAY[s][1][2]=="packets out") {
              dataToSend["value"] = new Long(message.net[i].opackets).toNumber();

              } else if (STAT_ARRAY[s][1][2]=="packets in") {
              dataToSend["value"] = new Long(message.net[i].ipackets).toNumber();
              } else {
                console.log("Unknown 'network' statistic");
              }
              break;
            }
          }
        } else if (STAT_ARRAY[s][1][0]=="disk") {
          dataToSend["value"] = 0;
          for (var i in message.disk) {
            if (message.disk[i].instance==STAT_ARRAY[s][1][3]) {
              // Allow for different [s][1][2]'s
              dataToSend["value"] = message.disk[i].reads;
              break;
            }
          }
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

    // Populate instances
    try {
      for (i in message.cpu) {
        if (instances[message.name][0].indexOf(message.cpu[i]["core"])==-1) {
          instances[message.name][0].push(message.cpu[i]["core"]);
        }
      }
      for (i in message.net) {
        if (instances[message.name][1].indexOf(message.net[i]["instance"])==-1) {
          instances[message.name][1].push(message.net[i]["instance"]);
        }
      }
      for (i in message.disk) {
        if (instances[message.name][2].indexOf(message.disk[i]["instance"])==-1) {
          instances[message.name][2].push(message.disk[i]["instance"]);
        }
      }
    } catch (err) {}

    saved_msgs[message.name]=message;
  });


  // Handle new message events 
  socket.on( 'message', function(message, flags) {

    // Init / ident message
    if (message["type"]=="init") {
      console.log(machine_list);
      socket.emit("message", {"type" : "machine_list", "list" : machine_list})
    } else if (message["type"]=="instance-req") {

      for (var n in machine_list) {
        if (Object.keys(instances).indexOf(machine_list[n])==-1) {
          instances[machine_list[n]] = [[], [], []];
        }
      }

      //if (message["info"][0]=="") return;
 
      data = [];

      if (message["info"][1]=="cpu") {
        console.log("instances");
        console.log(instances);
        console.log("msg[nfo]");
        console.log(message["info"]);
        console.log("failling call");
        console.log(instances[message["info"][0]]);
        data = instances[message["info"][0]][0];
      } else if (message["info"][1]=="network") {
        data = instances[message["info"][0]][1];
      } else if (message["info"][1]=="disk") {
        data = instances[message["info"][0]][2];
      }

      socket.emit("message", 
          {"type" : "instance-response", "data" : data});

    } else if (message["type"]=="request") {
      STAT_ARRAY.push([message["self"], message["info"]]); // { message["self"] : message["info"] } ); 
    }

  });

});
