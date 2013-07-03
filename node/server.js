/*
 *  server.js
 *    Standalong server that responds provides
 *    various system metrics when queried by
 *    clients.
 *
 *  DEPENDENCIES:
 *    node
 *    socket.io
 *    express
 *    libdtrace
 *    kstat
 *    (installable with npm)
 *
 *  USAGE:
 *    (sudo) node server.js           [user]
 *    (sudo) nohup node server.js &   [daemon]
 *
 *  CREATED:    14 JUNE 2013
 *  MODIFIED:    2 JULY 2013
 */

console.log("\n\033[00;31mServer starting...\033[00m \n")

/* Load libraries */
var sys = require('sys');
var exec = require('child_process').exec;
var kstat = require('kstat');
var libdtrace = require('libdtrace');
var http = require('http');
var libdtrace = require('libdtrace');
var express = require('express');
var scripts = require('./scripts.repo');

/* Initalized variables */
var script = {};    // The script to run (specified by "type" in original message)
var cmdScript = {}; // The string to give to cmdline
var nfo = {};       // Any accompanying information for the client (again, specified by "type")
var args;           // Argument array

/* Start up the express server */
var app = express(),
            http = require('http'),
            server = http.createServer(app),
            io = require('socket.io').listen(server);

/* Configure express server */
app.configure(function() {
  app.use(express.static(__dirname + '/public'));
});
io.set('log level', 1);

/* keep track of dtrace consumers and intervals */
var dtraceInt = {};
var dtp_list = {};
var retval = {};
var cmdInt = {};
var kInt = {};

/* Return well-formated time */
function returnTime() {
  var d = new Date();
  var h = d.getHours();
  var m = d.getMinutes();
  var s = d.getSeconds();

  if (h<10) { h = "0"+h; }
  if (m<10) { m = "0"+m; }
  if (s<10) { s = "0"+s; }

  return h+":"+m+":"+s; 
}

/* Insert a PID after loading from
 * the repository */
function addPID(f, pid) {
  if (!pid) return f;

  var p = f.indexOf("$#$");
  if (p<0) return f;

  return addPID(f.substr(0,p) + pid + f.substr(p+3), pid);
}

/* Insert an instance number after
 * loading from the repository */
function addINSTANCE(f, inst) {
  if (typeof(inst)==undefined) return f;

  for (var nfo in f) {
    if (nfo=="instance") f[nfo]=inst;
  }
  return f;
}

/* Insert a string after loading from
 * the repoistory */
function addSTRING(f, s) {
  if (!s) return f;

  var p = f.indexOf("$S$");
  if (p<0) return f;

  return addSTRING((f.substr(0,p) + s + f.substr(p+3)), s);
}

/* Searches through body for term and
 * returns a the first number that occurs
 * after the first instance of term
 */
function searchParse(body, term) {
  var line = "";
  var skips = 0;
  var terms = term.split(',');
  term = terms[0];

  if (terms.length > 1) skips = parseInt(terms[1]);
  
  var position = body.search(term);
  if (position < 0) return null;

  position += term.length;

  if (skips > 0) {
    while (body[position]!='\n') {
      line += body[position];
      position++;
    }

    line = line.split(' ');

    for (var parsed in line) {
      if (line[parsed].length!=0) {
        skips--;
      }
      if (skips<1) {
        return line[parsed];
      }
    }
  }

  while (isNaN(parseInt(body[position]))) {
    position++;
  }

  while (!isNaN(parseInt(body[position]))) {
    line += body[position];
    position++;
  }

  return line;
}

/* Talk to the belov'd user */
var dT = new Date();
console.log("\n \033[00;31m... online! \033[00m ")
console.log("\n \033[00;31mStart time was: " + dT.getDate() + "/" + 
  (dT.getMonth()+1) + "/" + dT.getFullYear() + " | " + returnTime() +
  "\033[00m \n");
  

/* Handle connection events */
io.sockets.on('connection', function(socket) { 

  /* Handle new message events */
  socket.on( 'message', function(message) {

    var prtQ = ' > \033[00;31m' + returnTime() +'  \033[01;36m' + 
      'connected\033[00m     [ \033[00;37m' + socket.id + '\033[00m' +
      ' ] < \033[00;35m' + message['type'] + ': \033[00;33m' + message["command"];

    for (var toAdd in message) {
      if (toAdd=="type") continue;
      if (toAdd=="vars") continue;
      if (toAdd=="command") continue;
      prtQ += ', ' +  message[toAdd];
    }

    prtQ += '\033[00m > ';
    console.log(prtQ);

    try {
      if (message['args']) {
        args = message['args'].substr(1);
      }   
    } catch (err) {
      console.log(err);
    }

    /* Script repository */
    nfo['loaddist'] = 4999;
    script['pzoom2'] = 'profile:::profile-4999\n/pid == ' + args +'/\n{\n@P[cpu,curthread] = count();\n' +
    '}';
    script['pzoom'] = 'syscall:::entry\n/pid == ' + args + '/\n{\n@P[probefunc,execname] = ' +
    'count();\nself->start = timestamp;\n}\n\nsyscall:::return\n /pid == ' + args + '/\n{\n' +
    'self->stop = timestamp;\n@Qtot[probefunc,execname] = sum(self->stop - self->start);\n' +
    '@Qavg[probefunc,execname] = avg(self->stop - self->start);\n@Qmin[probefunc,execname] = ' +
    'min(self->stop - self->start);\n@Qmax[probefunc,execname] = max(self->stop - self->start);\n}\n' +
    'profile:::profile-4999\n/pid == ' + args + '/\n{\n@Q[cpu,curthread] = count();\n}';

    /* This is for OSX */
    cmdScript['memStat'] = "top -l 1 | grep PhysMem:";

    /* Allows us to kill other scripts already running */
    if (message["type"] == "killer") {
      
      try {
        var toRemove = [];
        for (var j in Object.keys(dtraceInt)) {
          var key = Object.keys(dtraceInt)[j].split(',');
          if (key[0] == socket.id) {
            toRemove.push(key);
          }
        }
        try {
          for (var j in toRemove) {
            clearInterval( dtraceInt[toRemove[j]] );
          } 
        } catch (err) {
          console.log("Error killing process...");
        }
      } catch (err) {
        console.log("Il n'y en a rien de dtrace.");
    }

    /* Set up cmdline parsing script */
    } else if (message['type'] == "cmd") {
      var cmd = message['cmd'];

      cmdInt[[socket.id, message["command"]]] = setInterval( function() {
        var child = exec(scripts.cmd[cmd]['cmdl'], function (error, stdout, stderr) {
          var toSend = {};
          toSend['type'] = message['cmd'];

          for (var ret in scripts.cmd[cmd]['returns']) {
            toSend[ret] = searchParse(stdout, scripts.cmd[cmd]['returns'][ret]);
          }

          socket.emit('message', toSend);

          /* Get rid of cmdline instance if we receive another request from the same socket */
          socket.on('message', function (message) { 
            clearInterval(cmdInt[socket.id]);
          });

        }) 
      }, 1001);

    /* Statistics queries */
    } else if (message['type'] == "queryStats") {
      console.log(' > \033[00;31mWARN: [ \033[00;37m' + socket.id + 
      '\033[00;31m ] queried statistics\033[00m');
      
      for (var cnctn in cmdInt) {
        console.log('"cmd" instance from     [ \033[00;37m' + cnctn + '\033[00m ]');
      }
      for (var cnctn in dtraceInt) {
        console.log('"dtrace" instance from  [ \033[00;37m' + cnctn + '\033[00m ]');
      }
      for (var cnctn in kInt) {
        console.log('"kstat" instance from   [ \033[00;37m' + cnctn + '\033[00m ]');
      }

      socket.disconnect();  //  You could potentially spam the server with this 
                            //  command. This is an (annoying) safeguard.

    /* Setup kstat reader and execute */
    } else if (message["type"] == "kstat") {
      var aggdata = {};
      aggdata["type"] = message["command"];

      kInt[[socket.id, message["command"]]] = setInterval( function() {
        for (var stat in scripts.kscript[message["command"]]) {
          
          var kscript = scripts.kscript[message["command"]][stat];
          
          if (message["vars"]==2) {
            kscript = addINSTANCE(kscript, message["vars2"]);
            aggdata["type"] = message["command"].substr(0,message["command"].length-3) + message["vars2"];
          }

          var r = new kstat.Reader( kscript );
          aggdata[stat] = r.read()[0]["data"][stat];  
          r.close();
        }

        socket.emit("message", aggdata);
      }, 1001);

    /* Setup dtrace script runner and execute */
    } else {
      var dtp = new libdtrace.Consumer();

      if (message["vars"]==2) {
        try {
          dtp.strcompile(addSTRING(scripts.script[message["type"]],message["command"]));
        } catch (err) {
          console.log(err);
        }
      } else {
        try {
          dtp.strcompile(addPID(scripts.script[message['type']],message['command'])); 
        } catch (err) {
          console.log(err);
        }
      }

      if (message["print"] == 1) {
        try {
          console.log(addPID(scripts.script[message["type"]],message["command"]));
        } catch (err) {
          console.log(scripts.script[message["type"]]);
        }
      }

      dtp.go();
      dtp_list[[socket.id, message["type"]]] = dtp;

      /* Send information to client periodically */
      dtraceInt[[socket.id, message["type"]]] = setInterval(function () {
      
      try {
        var cpu = 0;
        var aggdata = {};
        dtp.aggwalk(function (id, key, val) {
          aggdata[key] = val;
          cpu += val;
        });

        aggdata['type'] = message['type'];
        aggdata['nfo'] = nfo[message['type']];
        aggdata['cpu'] = cpu;
        aggdata['mem'] = 64;
        socket.emit('message', aggdata);

        } catch( err ) {
          console.log(err);
        }
      }, 1001);
    }
  });

  /* Deal with disconnect from currently connected client */
  socket.on('disconnect', function(){ 
    try {
      console.log(' > \033[00;31m' + returnTime() + ' \033[01;36m ' +
        'disconnected\033[00m  [ \033[00;37m' + socket.id + '\033[00m ]');

      /* Try to clean up kstat intervals */
      try {
        var toRemove = [];
        for (var j in Object.keys(kInt)) {
          var key = Object.keys(kInt)[j].split(',');
          if (key[0] == socket.id) {
            toRemove.push(key);
          }
        }
        
        for (var j in toRemove) {
          clearInterval( kInt[toRemove[j]] );
          delete kInt[ toRemove[j] ];
        }
      } catch (err) {}

      /* Try to clean up cmd intervals */
      try {
        var toRemove = [];
        for (var j in Object.keys(cmdInt)) {
          var key = Object.keys(cmdInt)[j].split(',');
          if (key[0] == socket.id) {
            toRemove.push(key);
          }
        }

        for (var j in toRemove) {
          clearInterval( cmdInt[toRemove[j]] );
          delete cmdInt[ toremove[j] ];
        }
      } catch (err) {}  
        

      /* Try to clean up dtrace intervals */
      try {
        var toRemove = [];
        for (var j in Object.keys(dtraceInt)) {
          var key = Object.keys(dtraceInt)[j].split(',');
          if (key[0] == socket.id) {
            toRemove.push(key);
          }
        }
        
        for (var j in toRemove) {
          clearInterval( dtraceInt[toRemove[j]] );
          delete dtraceInt[ toRemove[j] ];
          
          try {
            var dtp = dtp_list[ toRemove[j] ];
            dtp.stop();
            delete dtp_list[ toRemove[j] ];
          } catch (err) {
            console.log("WARN: DTrace instance was not caught/killed");
          }
        }       
      } catch (err) {}

    } catch (err) {
    console.log(err);
    }
  });
});

/* Specify port number for server to listen on */
server.listen(4200);
