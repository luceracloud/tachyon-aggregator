// Sending commands in response to other commands.
// This example runs "type" against every key in the database
//
var client = require("redis").createClient(6380);
var multi = new Array();
var types = new Array();
var time = new Array();

function appendArray(pose, res, pos)
{


if(multi[pose] == undefined)
{
multi[pose] = new Array();
}

if(types[pose] == undefined)
{
types[pose] = pose;
}

multi[pose][pos] = res;

return;
}

function getStat(time1, time2, section, stat, type, value, want)
  {
  
  client.keys("*", function (err, keys) {

      keys.sort(function(a,b){return a - b});
      keys.forEach(function(key , pos)
      {
        console.log(key);  
        
        
       if(key >= time1 && key <= time2)
        {
       client.hgetall(key,  function (err, res) { 
       
        for(var str in res)
        {
        if(str.substring(0,3) == section)
            {
            if(str.substring(4,7) == stat.substring(0,3))
                 {
                  
                       if(type == 0)
                     {
                     if(res[str] >= value)
                     {
                         console.log(stat + " " + res[str]);
                         for (var sstr in res)
                              {
                           if(sstr.substring(0,3) == want)
                                {
                                  console.log(sstr + "   " +  res[sstr]);
                                }
                              }
                      } 
                   }
                   else
                   {
                   if(res[str] <= value)
                     {
                         console.log(stat + " " + res[str]);
                         for (var sstr in res)
                              {
                           if(sstr.substring(0,3) == want)
                                {
                                  console.log(res[sstr]);
                                }
                             }
                      }
                    }
                  
                 }
            }  
        }  
      
        });
        }
      
      });
    client.quit();
      }); 
    
  }

function callHeat() {

client.keys("*", function (err, keys) {

      keys.sort(function(a,b){return a - b});
 
      keys.forEach(function(key , pos)
      {
       client.hgetall(key,  function (err, res) {
            time.push(key);
            var  pose = "";
            for (var str in res)
            {
            if(str.substring(0,8) == "CallHeat" && str.substring(9,13) == "lowt")
              {
              pose = res[str];
              }
            
            if(str.substring(0,8) == "CallHeat" && str.substring(9,14) == "value")
              {
              appendArray(Number(pose), res[str], pos);
              pose = ""; 
              }
              }
              
            if (pos === (keys.length - 1)) { 
              console.log("times       " + time);
              for(var pose in types)
              {
              console.log(pose + " - " + (2* pose) + "             " +  multi[pose]);
              }
              }
            
            }); 
              
      });
    client.quit();

});
}

callHeat();
//getStat(0, 12250279085936882, "Dis", "writes", 0, 10, "Pro");
