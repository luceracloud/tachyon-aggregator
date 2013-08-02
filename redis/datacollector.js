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
console.log(process.argv);
var zmq = require('zmq');
var sock = zmq.socket('sub');
var redis = require("redis");
var ProtoBuf = require('protobufjs');
var Long = require("long");
var count = 0;

var directToRedis = "/root"; //Directory to the Redis folder

/* Connect to redis server */
client = redis.createClient();
sock.subscribe("");
sock.connect('tcp://127.0.0.1:7211');
console.log('\033[00;31mWorker connected to port 7200\033[00m');

/* Build communication protocol */
var Packet = ProtoBuf.protoFromFile("packet.proto").build("PB_MSG.Packet");

/* Variable inits and defns */
var time = 0;
var previous = new Array(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);

// Creates the Cron, which creates a new folder, pushes the data and the pushes the keys every minute
var cronJob = require('cron').CronJob;
var sys = require('sys');
var exec = require('child_process').exec;
new cronJob('0 * * * * *', function(){
  client.save();
  var ts = new Date().toTimeString().substring(0,8);
  var dater = new Date().toISOString().substring(0,10);
  var fileN = ts + "d.rdb";
  console.log(fileN);

  var cmd = "mkdir" + directToRedis  + "/redis/snapshots/" + dater; 
  var child = exec(cmd, function(error, stdout, stderr) { });

  var cmd2 = "cp " + directToRedis +  "/redis/mydump.rdb " + directToRedis + "/redis/snapshots/" + dater + "/" + fileN;

  console.log("command   " + cmd2);

  var child2 = exec(cmd2, function(error, stdout, stderr) { });
  var cmd3 = "rm " + directToRedis  + "/redis/mydump.rdb";
  var child3 = exec(cmd3, function(error, stdout, stderr) { });

  client.flushdb();

  console.log(ts);
}, null, true);

// Takes in the data from the c++ zmq server
sock.on('message', function(msg) {
  var message = Packet.decode(msg);
//  console.log(message);
  time = new Date().toTimeString().substring(0,8);

  var i = 0;
  var v1 = new Long(message.net[0]["obytes64_2"], message.net[0]["obytes64_1"]).toNumber();
  var v2 = new Long(message.net[0]["rbytes64_2"], message.net[0]["rbytes64_1"]).toNumber();
  var v3 = new Long(message.net[0]["opackets_2"], message.net[0]["opackets_1"]).toNumber();
  var v4 = new Long(message.net[0]["ipackets_2"], message.net[0]["ipackets_1"]).toNumber();

  console.log(count);

//Some data requires subtraction from previous data, so the first data set is not printed
  if(count > 0) {
    
    client.hmset("" + time, "Sys time", "" +  message.time["low"]);
    client.hmset("" + time, "Sys ticks", "" + message.ticks);

    for(var ice = message.process.length - 1; ice > 0 && ice > message.process.length - 40; ice--) {
      client.hmset("" + time, "Pro CPU " + ice, "" + message.process[ice]["cpu"]);
      client.hmset("" + time, "Pro PID " + ice, "" + message.process[ice]["pid"]);
      client.hmset("" + time, "Pro execname " + ice, "" + message.process[ice]["execname"]);
      client.hmset("" + time, "Pro Usage " + ice, "" + message.process[ice]["usage"]);
    }

    i = 0;
    while(typeof(message.cpu[i]) != "undefined") {
      client.hmset("" + time, "CPU core " + i, "" + message.cpu[i]["core"]);
      client.hmset("" + time, "CPU usage " + i, "" + message.cpu[i]["usage"]);
      i++;
    }

    client.hmset("" + time, "Mem rss", "" + new Long(message.mem[0]["rss_2"], message.mem[0]["rss_1"]).toNumber());
    client.hmset("" + time, "Mem memcap", "" + new Long(message.mem[0]["physcap_2"], message.mem[0]["physcap_1"]).toNumber());
    client.hmset("" + time, "Mem swap", "" + new Long(message.mem[0]["swap_2"], message.mem[0]["swap_1"]).toNumber());
    client.hmset("" + time, "Mem scap", "" + new Long(message.mem[0]["swapcap_2"], message.mem[0]["swapcap_1"]).toNumber());

    client.hmset("" + time, "Net obytes64", "" + (v1 - previous[0]));
    client.hmset("" + time, "Net rbytes64", "" + (v2 - previous[1]));
    client.hmset("" + time, "Net opackets", "" + (v3 - previous [2]));
    client.hmset("" + time, "Net ipackets", "" + (v4 - previous [3]));
    client.hmset("" + time, "Net instance", "" + message.net[0]["instance"]);
    client.hmset("" + time, "Net ierrors", "" + message.net[0]["ierrors"]);
    client.hmset("" + time, "Net oerrors", "" + message.net[0]["oerrors"]);  

    i = 0;
    while(typeof(message.callheat[i]) != "undefined") {
      client.hmset("" + time, "CallHeat name " + i, "" + message.callheat[i]["name"]);
      client.hmset("" + time, "CallHeat lowt " + i, "" + new Long(message.callheat[i]["lowt_2"], message.callheat[i]["lowt_1"]).toNumber());
      client.hmset("" + time, "CallHeat value " + i, "" + new Long(message.callheat[i]["value_2"], message.callheat[i]["value_1"]).toNumber());
      i++;
    }
  }
    
  previous[0] = v1;
  previous[1] = v2;
  previous[2] = v3;
  previous[3] = v4;
   
  i = 0;
  while(typeof(message.disk[i]) != "undefined") {
  
    var d1 = new Long(message.disk[i]["nread_2"], message.disk[i]["nread_1"]).toNumber();
    var d2 = new Long(message.disk[i]["nwritten_2"], message.disk[i]["nwritten_1"]).toNumber();
    var d3 = message.disk[i]["reads"];
    var d4 = message.disk[i]["writes"];
    var d5 = Math.floor(new Long(message.disk[i]["wtime_2"], message.disk[i]["wtime_1"]).toNumber()/1000) / 1000000;
    var d6 = Math.floor(new Long(message.disk[i]["wlentime_2"], message.disk[i]["wlentime_1"]).toNumber()/1000)/ 1000000;
   
    if(count > 0) {
      client.hmset("" + time, "Dis instance " + i, "" + message.disk[i]["instance"]);
      client.hmset("" + time, "Dis nread " + i, "" + (d1 - previous[6*(i) + 4]));
      client.hmset("" + time, "Dis nwritten " + i, "" + (d2 - previous[6*(i) + 5]));
      client.hmset("" + time, "Dis reads " + i, "" + (d3 - previous[6*(i) + 6]));
      client.hmset("" + time, "Dis writes " + i, "" + (d4 - previous[6*(i) + 7]));
      client.hmset("" + time, "Dis wtime " + i, "" + (d5 - previous[6*(i) + 8]));
      client.hmset("" + time, "Dis wlentime " + i, "" + (d6 - previous[6*(i) + 9]));
      client.hmset("" + time, "Dis harderror" + i, "" +  message.disk[i]["harderror"]);
      client.hmset("" + time, "Dis tranerror" + i, "" +  message.disk[i]["tranerror"]);
      client.hmset("" + time, "Dis softerror" + i, "" +  message.disk[i]["softerror"]);
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
