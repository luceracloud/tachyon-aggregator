/*
 *    example.js
 *
 *      Description TODO
 *
 *    CREATED:  15 JULY 2013
 *    UPDATED:  15 JULY 2013
 *
 */


/* Include relevant libraries */
var zmq = require('zmq')
var sock = zmq.socket('pull');
var redis = require("redis"),
var ProtoBuf = require('protobufjs');

/* Connect to redis server */
client = redis.createClient();
sock.connect('tcp://127.0.0.1:7200');
console.log('\033[00;31mWorker connected to port 7200\033[00m');

/* Build communication protocol */
var Packet = ProtoBuf.protoFromFile("packet.proto").build("Packet");

/* Variable inits and defns */
var database = new Array();
var time = 0;
var i = 0;

sock.on('message', function(msg){
  var message = Packet.decode(msg);
  database.push(message.time);

  time = message.time;  
  client.hmset("hash " + message.time, "ticks", "" + message.ticks);
  
  while(typeof(message.process[i]) != "undefined") {
    client.hmset("hash " + message.time, "Process PID", "" + message.process[i]["pid"]);
    client.hmset("hash " + message.time, "Process execname", "" + message.process[i]["execname"]);
    client.hmset("hash " + message.time, "Process Usage", "" + message.process[i]["usage"]);
    client.hmset("hash " + message.time, "Process PID", "" + message.process[i]["cpu"]);
    i++;
  }

  i = 0;
  while(typeof(message.cpu[i]) != "undefined") {
    client.hmset("hash " + message.time, "CPU core", "" + message.cpu[i]["core"]);
    client.hmset("hash " + message.time, "CPU usage", "" + message.cpu[i]["usage"]);
    i++;
  }

  i = 0;
  while(typeof(message.mem[i]) != "undefined") {
    client.hmset("hash " + message.time, "Mem physmem", "" + message.mem[i]["physmem"]);
    client.hmset("hash " + message.time, "Mem rss", "" + message.mem[i]["rss"]);
    client.hmset("hash " + message.time, "Mem pp_kernel", "" + message.mem[i]["pp_kernel"]);
    client.hmset("hash " + message.time, "Mem physcap", "" + message.mem[i]["physcap"]);
    client.hmset("hash " + message.time, "Mem freemem", "" + message.mem[i]["freemem"]);
    client.hmset("hash " + message.time, "Mem swap", "" + message.mem[i]["swap"]);
    client.hmset("hash " + message.time, "Mem swapcap", "" + message.mem[i]["swapcap"]);
    i++;
  }

  i = 0;
  while(typeof(message.net[i]) != "undefined") {
    client.hmset("hash " + message.time, "Net obytes64", "" + message.net[i]["obytes64"]);
    client.hmset("hash " + message.time, "Net rbytes64", "" + message.net[i]["rbytes64"]);
    client.hmset("hash " + message.time, "Net opackets", "" + message.net[i]["opackets"]);
    client.hmset("hash " + message.time, "Net ipackets", "" + message.net[i]["ipackets"]);
    client.hmset("hash " + message.time, "Net instance", "" + message.net[i]["instance"]);
    i++;
  }

  i = 0;
  while(typeof(message.disk[i]) != "undefined") {
    client.hmset("hash " + message.time, "Disk instance", "" + message.disk[i]["instance"]);
    client.hmset("hash " + message.time, "Disk nread", "" + message.disk[i]["nread"]);
    client.hmset("hash " + message.time, "Disk nwritten", "" + message.disk[i]["nwritten"]);
    client.hmset("hash " + message.time, "Disk reads", "" + message.disk[i]["reads"]);
    client.hmset("hash " + message.time, "Disk writes", "" + message.disk[i]["writes"]);
    client.hmset("hash " + message.time, "Disk wtime", "" + message.disk[i]["wtime"]);
    client.hmset("hash " + message.time, "Disk wlentime", "" + message.disk[i]["wlentime"]);
    i++;
  }
});

/* Handle errors */
client.on("error", function (err) {
  console.log("Error " + err);
});

/* Print out logged entries every 673 ms */
setInterval(function() {
  for(var i in database) {
    console.log(i);
    client.hgetall("hash " + database[i], function (err, obj) {
      console.log(obj);
    });
  }
}, 673);
