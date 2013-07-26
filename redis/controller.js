var client = require("redis").createClient(6378);
var cronJob = require('cron').CronJob;
var sys = require('sys');
var exec = require('child_process').exec;
var sent = 0;
var sentArr = ["cpu", "mem", "net", "dis"];

new cronJob('6,18,30,42,56 * * * * *', function(){
  client.save();
  var ts = new Date().getTime();
  var fileN = ts + "d.rdb";
  console.log(fileN);

  var cmd = "cp /opt/tools/redis/redis-2.6.14/src/thedump.rdb /opt/tools/redis/redis-2.6.14/src/" + sentArr[sent] + "/"  + fileN;
  sent = (sent +1) % 5;
  console.log("command   " + cmd);

  var child = exec(cmd, function(error, stdout, stderr) { });
  var cmd2 = "rm /opt/tools/redis/redis-2.6.14/src/thedump.rdb ";
  var child2 = exec(cmd2, function(error, stdout, stderr) { });

  client.keys("*", function (err, keys) {
    keys.forEach(function(key , pos) {
      client.migrate("localhost", 6381, key, 0, 1000);
    });
  });

  console.log(ts);
}, null, true);
