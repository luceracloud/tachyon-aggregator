/*
 *    example.js
 *
 *      Description TODO
 *
 *    CREATED:  15 JULY 2013
 *    UPDATED:  30 JULY 2013
 *
 */

/* Include relevant libraries */
console.log(process.argv);
var zmq = require('zmq');
var sock = zmq.socket('sub');
var redis = require("redis");
var ProtoBuf = require('protobufjs');
var Long = require("long");
var count = 0;
var sent = 0;
var sentArr = ["CPU", "MEM", "NET", "DIS", "CAL"];


/* Connect to redis server */
client = redis.createClient();
sock.subscribe("");
sock.connect('tcp://127.0.0.1:7211');
console.log('\033[00;31mWorker connected to port 7200\033[00m');

/* Build communication protocol */
var Packet = ProtoBuf.protoFromFile("packet.proto").build("PB_MSG.Packet");

/* Variable inits and defns */
var database = new Array();
var time = 0;
var previous = new Array(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);

// Creates the Cron, which creates a new folder, pushes the data and the pushes the keys every minute
var cronJob = require('cron').CronJob;
var sys = require('sys');
var exec = require('child_process').exec;
new cronJob('0,15,30,45 * * * * *', function(){
  client.save();
  var cmd2 = "rm /opt/tools/redis/redis-2.6.14/src/mydump.rdb ";
  var child2 = exec(cmd2, function(error, stdout, stderr) { });

  client.keys("*", function (err, keys) {
    keys.forEach(function(key , pos) {
      if(key.substring(key.length - 3) == sentArr[sent]) {
        client.migrate("localhost", 6378, key, 0, 1000);
      }
      if(pos == keys.length - 1)
      {
        sent = (sent + 1) % 5;
      }
    });
  });

}, null, true);

// Takes in the data from the c++ zmq server
sock.on('message', function(msg) {
  var message = Packet.decode(msg);
//  console.log(message);
  time = message.time["low"];  
  database.push(message.time["low"]);
 
  var i = 0;
  var v1 = new Long(message.net[0]["obytes64_2"], message.net[0]["obytes64_1"]).toNumber();
  var v2 = new Long(message.net[0]["rbytes64_2"], message.net[0]["rbytes64_1"]).toNumber();
  var v3 = new Long(message.net[0]["opackets_2"], message.net[0]["opackets_1"]).toNumber();
  var v4 = new Long(message.net[0]["ipackets_2"], message.net[0]["ipackets_1"]).toNumber();

  console.log(count);

//Some data requires subtraction from previous data, so the first data set is not printed
  if(count > 0); {

    client.hmset("" + time + "CPU", "Sys ticks", "" + message.ticks);

    while(typeof(message.process[i]) != "undefined") {
      client.hmset("" + time + "CPU", "Pro CPU " + i, "" + message.process[i]["cpu"]);
      client.hmset("" + time + "CPU", "Pro PID " + i, "" + message.process[i]["pid"]);
      client.hmset("" + time + "CPU", "Pro execname " + i, "" + message.process[i]["execname"]);
      client.hmset("" + time + "CPU", "Pro Usage " + i, "" + message.process[i]["usage"]);
      i++;
    }

    i = 0;
    while(typeof(message.cpu[i]) != "undefined") {
      client.hmset("" + time + "CPU", "CPU core " + i, "" + message.cpu[i]["core"]);
      client.hmset("" + time + "CPU", "CPU usage " + i, "" + message.cpu[i]["usage"]);
      i++;
    }

    client.hmset("" + time + "MEM", "Mem physmem", "" + new Long(message.mem[0]["physmem_2"],
                            message.mem[0]["physmem_1"]).toNumber());
    client.hmset("" + time + "MEM", "Mem rss", "" + new Long(message.mem[0]["rss_2"],
                            message.mem[0]["rss_1"]).toNumber());
    client.hmset("" + time + "MEM", "Mem pp_kernel", "" + new Long(message.mem[0]["pp_kernel_2"],
                            message.mem[0]["pp_kernel_1"]).toNumber());
    client.hmset("" + time + "MEM", "Mem memcap", "" + new Long(message.mem[0]["physcap_2"],
                            message.mem[0]["physcap_1"]).toNumber());
    client.hmset("" + time + "MEM", "Mem freemem", "" + new Long(message.mem[0]["freemem_2"],
                            message.mem[0]["freemem_1"]).toNumber());
    client.hmset("" + time + "MEM", "Mem swap", "" + new Long(message.mem[0]["swap_2"],
                            message.mem[0]["swap_1"]).toNumber());
    client.hmset("" + time + "MEM", "Mem scap", "" + new Long(message.mem[0]["swapcap_2"],
                            message.mem[0]["swapcap_1"]).toNumber());

    client.hmset("" + time + "NET", "Net obytes64", "" + (v1 - previous[0]));
    client.hmset("" + time + "NET", "Net rbytes64", "" + (v2 - previous[1]));
    client.hmset("" + time + "NET", "Net opackets", "" + (v3 - previous [2]));
    client.hmset("" + time + "NET", "Net ipackets", "" + (v4 - previous [3]));
    client.hmset("" + time + "NET", "Net instance", "" + message.net[0]["instance"]);
    client.hmset("" + time + "NET", "Net ierrors", "" + message.net[0]["ierrors"]);
    client.hmset("" + time + "NET", "Net oerrors", "" + message.net[0]["oerrors"]);


    i = 0;
    while(typeof(message.callheat[i]) != "undefined") {
      client.hmset("" + time + "CAL", "CallHeat name " + i, "" + message.callheat[i]["name"]);
      client.hmset("" + time + "CAL", "CallHeat lowt " + i, "" + new Long(message.callheat[i]["lowt_2"],
                            message.callheat[i]["lowt_1"]).toNumber());
      client.hmset("" + time + "CAL", "CallHeat value " + i, "" + new Long(message.callheat[i]["value_2"],
                            message.callheat[i]["value_1"]).toNumber());
      i++;
    }
  }
    
  previous[0] = v1;
  previous[1] = v2;
  previous[2] = v3;
  previous[3] = v4;
   
  i = 0;
  while(typeof(message.disk[i]) != "undefined") {
    client.hmset("" + time, "Dis instance " + i, "" + message.disk[i]["instance"]);
  
    var d1 = new Long(message.disk[i]["nread_2"], message.disk[i]["nread_1"]).toNumber();
    var d2 = new Long(message.disk[i]["nwritten_2"], message.disk[i]["nwritten_1"]).toNumber();
    var d3 = message.disk[i]["reads"];
    var d4 = message.disk[i]["writes"];
    var d5 = new Long(message.disk[i]["wtime_2"], message.disk[i]["wtime_1"]).toNumber();
    var d6 = new Long(message.disk[i]["wlentime_2"], message.disk[i]["wlentime_1"]).toNumber();
   
    if(count > 0) {
      client.hmset("" + time + "DIS", "Dis nread " + i, "" + (d1 - previous[6*(i) + 4]));
      client.hmset("" + time + "DIS", "Dis nwritten " + i, "" + (d2 - previous[6*(i) + 5]));
      client.hmset("" + time + "DIS", "Dis reads " + i, "" + (d3 - previous[6*(i) + 6]));
      client.hmset("" + time + "DIS", "Dis writes " + i, "" + (d4 - previous[6*(i) + 7]));
      client.hmset("" + time + "DIS", "Dis wtime " + i, "" + (d5 - previous[6*(i) + 8]));
      client.hmset("" + time + "DIS", "Dis wlentime " + i, "" + (d6 - previous[6*(i) + 9]));
      client.hmset("" + time + "DIS", "Dis harderror" + i, "" + message.disk[i]["harderror"]);
      client.hmset("" + time + "DIS", "Dis tranerror" + i, "" + message.disk[i]["tranerror"]);
      client.hmset("" + time + "DIS", "Dis softerror" + i, "" + message.disk[i]["softerror"]);
    } 
 
    previous[6*(i) + 4] = d1;
    previous[6*(i) + 5] = d2;
    previous[6*(i) + 6] = d3;
    previous[6*(i) + 7] = d4;
    previous[6*(i) + 8] = d5;
    previous[6*(i) + 9] = d6;
    i++;
  }  
  count++;
});

/* Handle errors */
client.on("error", function (err) {
  console.log("Error " + err);
});
