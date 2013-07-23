// Sending commands in response to other commands.
// This example runs "type" against every key in the database

// Edit the config Redis File to match the folder that is going to be queried
var fs = require('fs');

var a = fs.readdirSync('/opt/tools/redis/redis-2.6.14/src/snapshots');
  fs.open('/opt/tools/redis/redis-2.6.14/redis.conf4', 'a', function(err, fd) { 
  fs.write(fd, a[2], 5183, 18);
  fs.close(fd);
});

// Starts the Redis Server
var sys = require('sys');
var exec = require('child_process').exec;
var cmd = "sh /opt/tools/redis/redis-2.6.14/src/run2";
var child = exec(cmd, function(error, stdout, stderr) { });

// Creates the Redis Client and all other global variables needed
var client = require("redis").createClient(6380);
var util = require("util");
var multi = new Array();
var types = new Array();
var time = new Array();
var total = 0;
var max = 0;
var counts = 0;
var finished = false;

// For the CallHeat function. Creates the chart to display the quantization
function appendArray(pose, res, pos, size) {

  if (multi[pose] == undefined) {
    multi[pose] = new Array(size);
    for(var i=0; i<multi[pose].length; i++) {
      multi[pose][i] = 0;
    } 
  }

  if(types[pose] == undefined) {
    types[pose] = pose;
  }

  multi[pose][pos] = res;
  
  return;
}

// Returns the over/under of a specific stat, and returns a different section
function getStat(section, stat, type, value, want) {

  client.keys("*", function (err, keys) {
    keys.sort(function(a,b){return a - b});
    keys.forEach(function(key , pos) {
      client.hgetall(key,  function (err, res) { 

      // Goes through the key to find the specific stat 
      for(var str in res) {
        if(str.substring(0,3) == section) {
          if(str.substring(4,7) == stat.substring(0,3)) {
            //Checks to see if the stat is above or below (specified) the value
            if(type == 0) {
            if(res[str] >= value) {
              counts++;
              console.log(counts);
              console.log(key);
              console.log(stat + " " + res[str]);
              for (var sstr in res) {
                if(sstr.substring(0,3) == want) {
                  console.log(sstr + "   " +  res[sstr]);
                }
              }
            } 
          } else {
            if(res[str] <= value) {
              console.log(stat + " " + res[str]);
              for (var sstr in res) {
                if(sstr.substring(0,3) == want) console.log(res[sstr]);
              }
            }
          }

          }
        }  
      }

      //Shuts down Redis Client
      if(pos == keys.length -1) shutDown();

      });
    });
  });
}

// Generates Max, Average and counts for dataset

function genStats(section, stat, instance) {
  client.keys("*", function (err, keys) {
  keys.sort(function(a,b){return a - b});   

  keys.forEach(function(key , pos) {
  client.hgetall(key,  function (err, res) {

  //Searches for the specific stat               

  for(var str in res) {
    if(str.substring(0,3) == section) {
      if(str.substring(4,7) == stat.substring(0,3)) {                        

        // Add up the data
        var number = Number(res[str]);
  
        if(instance == undefined) {
          total += number;
          if(number > max) max = number;
          counts++;
        } else {
          if (str.substring(str.length - 2) == instance) {
            total += number;
            if(number > max) max = number;
            counts++;
          }  
        }
      }
    }
  }
  // Print the totals and close the redis server

  if(pos == keys.length -1) {              
    if(instance == undefined) {
      console.log("average " + section + " " + stat + " " + (total/counts));
    }
    if(instance != undefined) {
      console.log("average " + section + " " + stat + " Instance" + instance  + ": " + (total/counts));
    }
    console.log("max   " + max); 
    console.log("count " + (counts));
    shutDown(); 
  }
  });      
  });
  });
}

function shutDown() {
  client.shutdown();
  client.quit();
}

//Print on quantized chart
function callHeat() {
  client.keys("*", function (err, keys) {
    keys.sort(function(a,b){return a - b});
    keys.forEach(function(key , pos) {
      // Print out quantization
      client.hgetall(key,  function (err, res) {
        time.push(key);
        var  pose = "";
        for (var str in res) {
          if(str.substring(0,8) == "CallHeat" && str.substring(9,13) == "lowt") {
            pose = res[str];
          }

          if(str.substring(0,8) == "CallHeat" && str.substring(9,14) == "value") {
            appendArray(Number(pose), res[str], pos, keys.length);
            pose = ""; 
          }
        }

        if (pos == (keys.length - 1)) { 
          console.log("times       " + time);
          for(var pose in types) {
            console.log(pose + " - " + (2* pose) + "             " +  multi[pose]);
          }
          shutDown();
        }
      }); 
    });
  });
}

// Straight up prints the data for a given time
function printData(times) {
  client.keys("*", function (err, keys) {
    keys.sort(function(a,b){return a - b});
    times.forEach(function(time , pos) {
      client.hgetall(keys[time],  function (err, res) {
      console.log(keys[time]);
      console.log(res);
      if(pos == times.length -1) shutDown();
      });
    });
  });
}

var arr = [10, 12];
getStat("Net", "rbytes64", 0, 4000, "Net");
