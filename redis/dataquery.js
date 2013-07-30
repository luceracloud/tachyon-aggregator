// This example runs "type" against every key in the database
// Edit the config Redis File to match the folder that is going to be queried
var fs = require('fs');
var sys = require('sys');
var exec = require('child_process').exec;

// Creates the Redis Client and all other global variables needed
var c = fs.readdirSync('/opt/tools/redis/redis-2.6.14/src/snapshots');
c.sort();
var total = 0;
var max = 0;
var counts = 0;
var stand = new Array();
var multi = new Array();
var types = new Array();
var time = new Array();
var numCores = 16;
var quant = new Array();
var cpu = new Array(numCores);
var client;
var args = 0;
var calls = [];
var arr = new Array(calls.length);
var begin = ["Dis","wtime", true, 1];
doOnAll(genStats, begin);

function startServer (data) {
  i = data[0];
  console.log();
  console.log("Start Server file    " + c[i]);
  callback = data[1];
  cb = data[2];
  fs.open('/opt/tools/redis/redis-2.6.14/redis.conf2', 'a', function(err, fd) {
    fs.writeSync(fd, c[i], 5183, 18);
    fs.closeSync(fd);
    callback (cb);
  });
}

function createServer1 (cb) {
  var cmd = "sh /opt/tools/redis/redis-2.6.14/src/run2";
  var child = exec(cmd, function(error, stdout, stderr) {
  });
  cb();
}

function createServer2 (cb) {
  var cmd2 = "sh /opt/tools/redis/redis-2.6.14/src/run2";
  var child2 = exec(cmd2, function(error, stdout, stderr) {
  });
  setTimeout(cb, 50);
}

function createClient() {
  client = require("redis").createClient(6380);
  var func = calls[0];
  calls.splice(0,1);
  func(arr[args++] , calls);
}

// Gets the stats for a given function over all files
 function doOnAll(func, param) {

   for(var i = 0; i < c.length - 1; i++) {
     calls.push(func);
     calls.push(shutDown);
     calls.push(startServer);
     arr.push(param);
     arr.push([]);
     arr.push([i+1, createServer2, createClient]);
   }
   calls.push(func);
   calls.push(end);
   arr.push(param);
   arr.push();
   startServer([0, createServer1, createClient]);
 }


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

function addQuantize(pos)
{
  if(quant[pos] == undefined) {
    quant[pos] = 0;
  }
  quant[pos]++;
  return;
}

function addCPU(cp, val, pos, size)
{

  if(cpu[cp] == undefined) {
  cpu[cp] = new Array(size);
  for(var i = 0; i < cpu[cp].length; i++)
    {
    cpu[cp][i] = 0;
    }
  }

  cpu[cp][pos] = val;
  return;

}

// Returns the over/under of a specific stat, and returns a different section
// [Section (Mem, CPU) , Stat (rbytes64, usage), type (0 for >=, 1 for <=),
// value(5000,0, 2)]
function getStat(data, cb) {

  var section = data[0];
  var stat = data[1];
  var type = data[2];
  var value = data[3];
  var instance = data[4];
  client.keys("*", function (err, keys) {
  keys.sort(function(a,b){return a - b});
  keys.forEach(function(key , pos) {
  client.hgetall(key,  function (err, res) { 

  // Goes through the key to find the specific stat 
  for(var str in res) {
    if(str.substring(0,3) == section) {
      if(str.substring(4,7) == stat.substring(0,3)) {
        if(str.substring(str.length - 2) == instance) {
        //Checks to see if the stat is above or below (specified) the value
        if(type == 0) {
        if(res[str] >= value) {
          console.log(key);
          console.log(stat + " " + res[str]);
          for (var sstr in res) {
            if(sstr.substring(0,3) == section) {
              if(instance == undefined) {
              console.log(sstr + "   " +  res[sstr]);
              }
              else {
                if (sstr.substring(sstr.length - 2) == instance) {
                  console.log(sstr + "   " +  res[sstr]);
                }
              }
            }
          }
        } 
      } else {
        if(res[str] <= value) {
          console.log(key);
          console.log(stat + " " + res[str]);
          for (var sstr in res) {
            if(instance == undefined) {
              console.log(sstr + "   " +  res[sstr]);
              }
            else {
              if (str.substring(str.length - 2) == instance) {
              console.log(sstr + "   " +  res[sstr]);
              }
            }
          }
        }
       }
      }
    }
  }  
}
      if (pos == keys.length -1) {
        var func = cb[0];
          cb.splice(0,1);
          if(cb.length == 0) {
            func(arr[args++]);
          }
          else {
            func(arr[args++], cb);
          }
        }
      });
    });
  });
}

// Generates Max, Average and counts for dataset
// [Section (Mem, Net), Stat (rbytes64, usage) , instance (0)]

function genStats(data, cb) {
  section = data[0];
  stat = data[1];
  computeForAll = data[2];
  instance = data[3];
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
          stand[counts] = number;
          counts++;
        } else {
          if (str.substring(str.length - 2) == instance) {
            total += number;
            if(number > max) max = number;
            stand[counts] = number;
            counts++;
          }  
        }
      }
    }
  }
  // Print the totals and close the redis server

  if(pos == keys.length -1) {
    if(!computeForAll || cb.length == 1) {
    var averageA = total/counts;              
    if(instance == undefined) {
      console.log("average " + section + " " + stat + " " + averageA);
    }
    if(instance != undefined) {
      console.log("average " + section + " " + stat + " Instance" + instance  + ": " + (total/counts));
    }
    console.log("max          " + max); 
    console.log("count        " + (counts));
    var totalStand = 0;
    for(var i = 0; i < stand.length; i++) {
      totalStand += (stand[i] - averageA)*(stand[i] - averageA);
    }
    console.log("Standard Deviation    " + Math.sqrt(totalStand/counts));
    total = 0;
    max = 0;
    counts = 0;
    stand = new Array();
    } 
    
    var func = cb[0];
    cb.splice(0,1);
    if(cb.length == 0) {
      func(arr[args++]);
      }
    else {
      func(arr[args++], cb);
      } 
    
  }
  });      
  });
  });
}

function shutDown(data, cb) {
  client.shutdown();
  client.quit();
  var func = cb[0];
  cb.splice(0,1);
  if(cb.length == 0) {
    func(arr[args++]);
    }
  else {
    func(arr[args++], cb);
    }
}

function end() {
  client.shutdown();
  client.quit();
}


//Print on quantized chart
//No Data
function callHeat(data, cb) {
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
          multi = new Array(); 
          types = new Array();
          time = new Array();
          var func = cb[0];
          cb.splice(0,1);
          if(cb.length == 0) {
            func(arr[args++]);
          }
          else {
            func(arr[args++], cb);
          }
        }
      }); 
    });
  });
}

//Create a quantization for a given data set
// [Section (Mem, Net), Stat (rbytes64, usage) , instance (0, optional)]
function quantize(data, cb) {

  section = data[0];
  stat = data[1];
  instance = data[2];
  client.keys("*", function (err, keys) {
    keys.forEach(function(key , pos) {
      client.hgetall(key,  function (err, res) {
        for(var str in res) {
          if(str.substring(0,3) == section) {
            if(str.substring(4,7) == stat.substring(0,3)) {
              if(instance == undefined) {
              var i = 1; 
              var number = Number(res[str]);
              if(number == 0) addQuantize(0);
              else
                {
                while(number >= i) {
                  i *= 2;
                  }
                addQuantize(i/2);
                }
              } 
              else {
                if (str.substring(str.length - 2) == instance) {
                  var i = 1;
                  var number = Number(res[str]);
                  if(number == 0) addQuantize(0);
                  else {
                    while(number >= i) {
                      i *= 2;
                    }
                  addQuantize(i/2);
                  }
                }
              }
            }
          }
        }
        if (pos == (keys.length - 1)) {
          for(var pose in quant) {
            console.log(pose + " - " + (2* pose) + "             " +  quant[pose]);
          }
          quant = [];
          var func = cb[0];
          cb.splice(0,1);
          if(cb.length == 0) {
            func(arr[args++]);
          }
          else {
            func(arr[args++], cb);
          }
        }
      });
    });
  });
}

//Print out the CPU Usage stats
//No input
function cpuStat(data, cb)
{
  client.keys("*", function (err, keys) {
    keys.sort(function(a,b){return a - b});
    keys.forEach(function(key , pos) {
      client.hgetall(key,  function (err, res) {
        time.push(key);
        var cp = 0;
        var val = 0;
        var ticks = 0;
        for(var str in res) {
          if(str.substring(0,3) == "Sys" && str.substring(4,9) == "ticks") {
            var ticks = res[str];
          }
          if(str.substring(0,3) == "CPU" && str.substring(4,8) == "core") {   
            cp = res[str];
          }
          if(str.substring(0,3) == "CPU" && str.substring(4,9) == "usage") {
            val = res[str];
            val = Math.floor(val* 10000 / ticks) / 100;
            addCPU(cp, val, pos, keys.length);
          }
        }
        if (pos == (keys.length - 1)) {
          console.log("times       " + time);
          for(var cp in cpu) {
            console.log("cpu " +  cp  + "             " +  cpu[cp]);
          }
          time = [];
          cpu = new Array(numCores);
          var func = cb[0];
          cb.splice(0,1);
          if(cb.length == 0) {
            func(arr[args++]);
          }
          else {
            func(arr[args++], cb);
          }
        }
      });
    });
  });
}

//Print out all the processes run on the given CPU
//[CPU A, CPU B ..... ]
function myprocess(data, cb)
{
  client.keys("*", function (err, keys) {
    keys.sort(function(a,b){return a - b});
    keys.forEach(function(key , pos) {
      client.hgetall(key,  function (err, res) {
        var b = false;
        var pose = "";
        var ticks = 0;
        for(var str in res) {
          if(str.substring(0,3) == "Sys" && str.substring(4,9) == "ticks") {
          ticks = res[str];
          }
          if(str.substring(0,3) == "Pro" && str.substring(4,7) == "CPU") {
            for (var c in data) {
            if(data[c] == str.substring(str.length - 2)) {
                b = true;
                pose += "CPU " + data[c];
              }
            }
          }
          if(b && str.substring(0,3) == "Pro" && str.substring(4,7) == "PID") {
            pose += "    PID " + res[str];
          }
          if(b && str.substring(0,3) == "Pro" && str.substring(4,7) == "exe") {
            pose += "    execname " + res[str];
          }
          if(b && str.substring(0,3) == "Pro" && str.substring(4,7) == "Usa") {
            pose += "     usage " + Math.floor(res[str]* 10000 / ticks) / 100  + "    time " + key;
            console.log(pose);
            b = false;
            pose = "";
          }
        }
        if(pos == keys.length - 1) {
          var func = cb[0];
          cb.splice(0,1);
          if(cb.length == 0) {
            func(arr[args++]);
          }
          else {
            func(arr[args++], cb);
          }
        }
      });      
    });
  });
}

// Straight up prints the data for a given time
// [Time A, Time B ...... ]
function printData(times, cb) {
  client.keys("*", function (err, keys) {
    keys.sort(function(a,b){return a - b});
    times.forEach(function(time , pos) {
      client.hgetall(keys[time],  function (err, res) {
      console.log(keys[time]);
      console.log(res);
      if(time == keys.length -1) {
        var func = cb[0];
          cb.splice(0,1);
          if(cb.length == 0) {
            func(arr[args++]);
          }
          else {
            func(arr[args++], cb);
          }
        }
      });
    });
  });
}

// Lists four main memory stats across all times
function getMemStats(data, cb) {

  client.keys("*", function (err, keys) {
    keys.sort(function(a,b){return a - b});
    keys.forEach(function(key, pos) {
      client.hgetall(key,  function (err, res) {
        var memA = new Array();
        for(var str in res) {
          if(str.substring(0,3) == "Mem") {
          memA[str.substring(4,7)] = res[str];    
          }
        }
        
        appendArray("mem", Math.floor(((memA["phy"] - memA["fre"])/memA["phy"])*1000)/10, pos, keys.length);
        appendArray("swap", Math.floor(memA["swa"]/memA["sca"]*1000)/10, pos, keys.length); 
        appendArray("kernal", Math.floor(memA["pp_"]/ memA["phy"] * 1000)/ 10, pos, keys.length);
        appendArray("rss", Math.floor(memA["rss"]/ memA["mem"] * 1000)/ 10, pos, keys.length);       
 
        if(pos == keys.length - 1) {
          for(var pose in types) {
            console.log(pose +  "             " +  multi[pose]);
          }
          multi = new Array();
          types = new Array();
          var func = cb[0];
          cb.splice(0,1);
          if(cb.length == 0) {
            func(arr[args++]);
          }
          else {
            func(arr[args++], cb);
          }
        }

      });
    });
  });
}

//Finds an error in the file
function findError(data, cb) {
  var errs = new Array();
  client.keys("*", function (err, keys) {
    keys.sort(function(a,b){return a - b});
    keys.forEach(function(key, pos) {
      client.hgetall(key,  function (err, res) {
      for (var str in res)
        {
        if(str.substring(8, 13) == "error" || str.substring(5,10) == "error") {
          if(pos == 0) {
            errs[str] = res[str];
          }
          else {
            if(errs[str] != res[str]) {
              if(errs[str] == undefined) {
              errs[str] = res[str];
              }
              else {
              console.log(pos);
              console.log(errs[str]);
              errs[str] = res[str];
              console.log("ERROR TYPE " + str + "  amount   " + res[str]);
              }
            }
          }
        }   
       }
        if(pos == keys.length -1)
          {
            var func = cb[0];
          cb.splice(0,1);
          if(cb.length == 0) {
            func(arr[args++]);
          }
          else {
            func(arr[args++], cb);
          }
          }     
      });
    });
  });
}
