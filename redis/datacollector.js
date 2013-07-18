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
var redis = require("redis");
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
var previous = new Array(82);



var cronJob = require('cron').CronJob;
var sys = require('sys');
var exec = require('child_process').exec;

new cronJob('0 * * * * *', function(){

client.save();

var ts = new Date().getTime();
var fileN = ts + "d.rdb"
console.log(fileN);

var cmd = "cp /opt/tools/redis/redis-2.6.14/src/mydump.rdb /opt/tools/redis/redis-2.6.14/src/snapshots/" + f

console.log("command   " + cmd);

var child = exec(cmd, function(error, stdout, stderr)
{

});

var cmd2 = "rm /opt/tools/redis/redis-2.6.14/src/mydump.rdb "

var child2 = exec(cmd2, function(error, stdout, stderr)
{

});

client.keys("*", function (err, keys) {

keys.forEach(function(key , pos)
{

client.migrate("localhost", 6381, key, 0, 1000);

});

});


console.log(ts);

}, null, true);

for(var i in previous)
{
previous[i] = 0;
console.log(previous[i][0]);
}


sock.on('message', function(msg){
  var message = Packet.decode(msg);

  time = message.time;  
  console.log(message.time);
  database.push(message.time);
  client.hmset("" + message.time, "Sys ticks", "" + message.ticks);
  
  var i = 0;

  while(typeof(message.process[i]) != "undefined") {
    client.hmset("" + message.time, "Pro PID " + i, "" + message.process[i]["pid"]);
    client.hmset("" + message.time, "Pro execname " + i, "" + message.process[i]["execname"]);
    client.hmset("" + message.time, "Pro Usage " + i, "" + message.process[i]["usage"]);
    client.hmset("" + message.time, "Pro CPU " + i, "" + message.process[i]["cpu"]);
    i++;
  }

  i = 0;
  while(typeof(message.cpu[i]) != "undefined") {
    client.hmset("" + message.time, "CPU core " + i, "" + message.cpu[i]["core"]);
    client.hmset("" + message.time, "CPU usage " + i, "" + message.cpu[i]["usage"]);
    i++;
  }

    client.hmset("" + message.time, "Mem physmem", "" + message.mem["physmem"]);
    client.hmset("" + message.time, "Mem rss", "" + message.mem["rss"]);
    client.hmset("" + message.time, "Mem pp_kernel", "" + message.mem["pp_kernel"]);
    client.hmset("" + message.time, "Mem physcap", "" + message.mem["physcap"]);
    client.hmset("" + message.time, "Mem freemem", "" + message.mem["freemem"]);
    client.hmset("" + message.time, "Mem swap", "" + message.mem["swap"]);
    client.hmset("" + message.time, "Mem scap", "" + message.mem["scap"]);

    client.hmset("" + message.time, "Net obytes64", "" + (message.net[0]["obytes64"] - previous[0]));
    previous[0] = message.net[0]["obytes64"];
    client.hmset("" + message.time, "Net rbytes64", "" + (message.net[0]["rbytes64"] - previous[1]));
    previous[1] = message.net[0]["rbytes64"];
    client.hmset("" + message.time, "Net opackets", "" + (message.net[0]["opackets"] - previous [2]));
    previous[2] = message.net[0]["opackets"];
    client.hmset("" + message.time, "Net ipackets", "" + (message.net[0]["ipackets"] - previous [3]));
    previous[3] = message.net[0]["ipackets"];
    client.hmset("" + message.time, "Net instance", "" + message.net[0]["instance"]);

  i = 0;
  while(typeof(message.disk[i]) != "undefined") {
    client.hmset("" + message.time, "Dis instance " + i, "" + message.disk[i]["instance"]);
    client.hmset("" + message.time, "Dis nread " + i, "" + (message.disk[i]["nread"] - previous[4*(i+1)]));i
    previous[4*(i+1) + 0] = message.disk[i]["nread"];
    client.hmset("" + message.time, "Dis nwritten " + i, "" + (message.disk[i]["nwritten"] - previous[4*(i+1) + 1]));
    previous[4*(i+1) + 1] = message.disk[i]["nwritten"];
    client.hmset("" + message.time, "Dis reads " + i, "" + (message.disk[i]["reads"] - previous[4*(i+1) + 2]));
    previous[4*(i+1) + 2] = message.disk[i]["reads"];
    client.hmset("" + message.time, "Dis writes " + i, "" + (message.disk[i]["writes"] - previous[4*(i+1) + 3]));
    previous[4*(i+1) + 3] = message.disk[i]["writes"];
    client.hmset("" + message.time, "Dis wtime " + i, "" + (message.disk[i]["wtime"] - previous[4*(i+1) + 4]));
    previous[4*(i+1) + 4] = message.disk[i]["wtime"];
    client.hmset("" + message.time, "Dis wlentime " + i, "" + (message.disk[i]["wlentime"] - previous[4*(i+1) + 5]));
    previous[4*(i+1) + 5] = message.disk[i]["wlentime"];
    i++;
  }
  
  i = 0;
  while(typeof(message.callheat[i]) != "undefined") {
    client.hmset("" + message.time, "CallHeat name " + i, "" + message.callheat[i]["name"]);
    client.hmset("" + message.time, "CallHeat lowt " + i, "" + message.callheat[i]["lowt"]);
    client.hmset("" + message.time, "CallHeat hight " + i, "" + message.callheat[i]["hight"]);
    client.hmset("" + message.time, "CallHeat value " + i, "" + message.callheat[i]["value"]);
    i++;
  }

});

/* Handle errors */
client.on("error", function (err) {
  console.log("Error " + err);
});

/* Print out logged entries every 673 ms 
setInterval(function() {
  for(var i in database) {
    client.hgetall("" + database[i], function(err,res) {
            
            console.log(res);

            });    
}
  
}, 673);*/
