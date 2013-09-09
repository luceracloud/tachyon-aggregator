// API
// ===

module.exports.api = function(server, io) {

  var machine_list = ["gz1", "gz2", "ngzA", "ngzB"];

  /* Handling zmq/protobuf parsing */
  //var zmq = require('zmq');
  //var sock = zmq.socket('sub');
  //var ProtoBuf = require('protobufjs');
  //var Long = require('long');
  //var Packet = ProtoBuf.protoFromFile("pckt.proto").build("PBMSG.Packet");

  //sock.subscribe("");
  //sock.connect('tcp://172.20.0.85:7211');



  /* Handling http requests */
  io.sockets.on('connection', function(socket) {

      
    var instances = {  
      "gz1": [
        [0, 1],
        ['ixgbe0', 'igb0'],
        ["zone_zfs"]
      ],
        "gz2": [
        [0, 1, 2, 3],
        ['ixgbe0', 'igb0'],
        ["zone_zfs", "zone_vfs"]
      ],
        "ngzA": [
        [0],
        ['ixgbe0', 'igb0'],
        ["abc"]
      ],
        "ngzB": [
        [0, 1, 2, 3, 4, 5, 6, 7],
        ['ixgbe0', 'igb0'],
        ["zyx"]
      ]
    };

      
    STAT_ARRAY = [];

      
    setInterval(function() {    
      for (s in STAT_ARRAY) {      
        var datum = {};      
        datum["type"] = "value-response";      
        datum["plot"] = STAT_ARRAY[s][0];      
        datum["key"] = [STAT_ARRAY[s][1][1],                       STAT_ARRAY[s][1][2],                       STAT_ARRAY[s][1][3]];      
        datum["modify"] = 0;      
        datum["value"] = Math.random() * 11;      
        socket.emit("message", datum);    
      }  
    }, 1000);


       /* Start listening to zmq/protobuf */

         // Populate instances
        
    /*
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
    */

       // Handle new message events
      socket.on('message', function(message, flags) {

           // Init / ident message
          
      if (message["type"] == "init") {      
        socket.emit("message", {
          "type": "machine_list",
          "list": machine_list
        })    
      } else if (message["type"] == "instance-req") {
        console.log("=======");
        console.log("recv instance req");
        console.log(message);
              
        for (var n in machine_list) {        
          if (Object.keys(instances).indexOf(machine_list[n]) == -1) {          
            instances[machine_list[n]] = [
              [],
              [],
              []
            ];        
          }      
        }

              
        data = [];

              
        if (message["info"][1] == "cpu") {        
          data = instances[message["info"][0]][0];      
        } else if (message["info"][1] == "network") {        
          data = instances[message["info"][0]][1];      
        } else if (message["info"][1] == "disk") {        
          data = instances[message["info"][0]][2];      
        }

              
        socket.emit("message",            {
          "type": "instance-response",
          "data": data,
          "self": message["info"][2]
        });

            
      } else if (message["type"] == "request") {      
        STAT_ARRAY.push([message["self"], message["info"]]);
       
      } else if (message["type"] == "cancel") { 
        for (s = STAT_ARRAY.length; s--;) {
          console.log(STAT_ARRAY[s]);
          if (STAT_ARRAY[s][0] == message["plt"]) {
            if (STAT_ARRAY[s][1][1] == message['info'][0]) {
              if (STAT_ARRAY[s][1][2] == message['info'][1]) {
                if (STAT_ARRAY[s][1][3] == message['info'][2]) {
                  console.log(message);
                  console.log("^^ MSG :: INTERNAL vv");
                  console.log(STAT_ARRAY[s]);
                  STAT_ARRAY.splice(s, 1);
                }
              }
            }
          }      
        }    
      } else {      
        console.log("Received unknown message type. Contents:");      
        console.log(message);    
      }

        
    });

  });

};
