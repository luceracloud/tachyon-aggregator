// API
// ===

module.exports.api = function(server, io) {

  var machine_list = [];

  /* Handling zmq/protobuf parsing */
  var zmq = require('zmq');
  var sock = zmq.socket('sub');
  var ProtoBuf = require('protobufjs');
  var Long = require('long');
  var Packet = ProtoBuf.protoFromFile("pckt.proto").build("PBMSG.Packet");

  sock.subscribe("");
  sock.connect('tcp://10.20.1.35:7211');

  io.set('log level', 1);

  /* Handling http requests */
  io.sockets.on('connection', function(socket) { 

    var instances = {}; // key{zone; val{[cpu,net,disk]
    STAT_ARRAY = [];

    /* Start listening to zmq/protobuf */
    sock.on("message", function(msg) {
      var message;

      console.log("RECV PROTOBUF");

      try {
        message = Packet.decode(msg);
        //console.log(message);
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
                              STAT_ARRAY[s][1][3]];
          dataToSend["modify"] = 0; // flag set to 1 if we want subtraction
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
                dataToSend["modify"] = 1;
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
            dataToSend["modify"] = 1;
            for (var i in message.disk) {
              if (message.disk[i].instance==STAT_ARRAY[s][1][3]) {
                dataToSend["value"] = 0;
                if (STAT_ARRAY[s][1][2]=="number of reads") {
                  dataToSend["value"] = message.disk[i].reads;
                } else if (STAT_ARRAY[s][1][2]=="number of writes") {
                  dataToSend["value"] = message.disk[i].writes;
                } else if (STAT_ARRAY[s][1][2]=="bytes read") {
                  dataToSend["value"] = new Long(message.disk[i].nread).toNumber();;
                } else if (STAT_ARRAY[s][1][2]=="bytes written") {
                  dataToSend["value"] = new Long(message.disk[i].nwritten).toNumber();
                } else if (STAT_ARRAY[s][1][2]=="run time") {
                  dataToSend["value"] = new Long(message.disk[i].rtime).toNumber();
                } else if (STAT_ARRAY[s][1][2]=="wait time") {
                  dataToSend["value"] = new Long(message.disk[i].wtime).toNumber();
                } else if (STAT_ARRAY[s][1][2]=="total run time") {
                  dataToSend["value"] = new Long(message.disk[i].rlentime).toNumber();
                  // total rlentime
                } else if (STAT_ARRAY[s][1][2]=="total wait time") {
                  dataToSend["value"] = new Long(message.disk[i].wlentime).toNumber();
                } else {
                  console.log("Unknown 'network' statistic");
                }

  //
  //   optional uint32 harderror = 10;
  //   optional uint32 softerror = 11;
  //   optional uint32 tranerror = 12;
  //
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

    });


    // Handle new message events 
    socket.on('message', function(message, flags) {

      // Init / ident message
      if (message["type"]=="init") {
        socket.emit("message", {"type" : "machine_list", "list" : machine_list})
      } else if (message["type"]=="instance-req") {

        for (var n in machine_list) {
          if (Object.keys(instances).indexOf(machine_list[n])==-1) {
            instances[machine_list[n]] = [[], [], []];
          }
        }
        console.log(instances); 
        data = [];

        if (message["info"][1]=="cpu") {
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
      } else if (message["type"]=="cancel") {
        console.log(message);
        console.log("~+~+~+~+~+~+~+~+~+~");
        for (s=STAT_ARRAY.length; s--;) {
          console.log(STAT_ARRAY[s]);
          if (STAT_ARRAY[s][0]==message["plt"]) {
            if (STAT_ARRAY[s][1][1]=="null") {}
            STAT_ARRAY.splice(s,1);
          }
        }
      } else {
        console.log("Received unknown message type. Contents:");
        console.log(message);
      }

    });

  });

};
