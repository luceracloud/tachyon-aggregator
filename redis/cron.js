var cronJob = require('cron').CronJob;
var sys = require('sys');
var exec = require('child_process').exec;

new cronJob('0 * * * * *', function(){

var ts = new Date().getTime();
var f = ts + "d.rdb"
console.log(f);

var cmd = "cp /opt/tools/redis/redis-2.6.14/src/mydump.rdb /opt/tools/redis/redis-2.6.14/src/snapshots/" + f

console.log("command   " + cmd); 

var child = exec(cmd, function(error, stdout, stderr)
{

});

var cmd2 = "rm /opt/tools/redis/redis-2.6.14/src/mydump.rdb "

var child2 = exec(cmd2, function(error, stdout, stderr)
{

});

console.log(ts);

}, null, true);
