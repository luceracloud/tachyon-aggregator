/*
 *	server.js
 *		Standalong server that responds provides
 *		various system metrics when queried by
 *		clients.
 *
 *	DEPENDENCIES:
 *		node
 *		socket.io
 *		express
 *		libdtrace
 *	  (installable with npm)
 *
 *	USAGE:
 *		(sudo) node server.js			[user]
 *		(sudo) nohup node server.js &	[daemon]
 *
 *	CREATED:    14 JUNE 2013
 *	MODIFIED:   20 JUNE 2013
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
var script = {};	// The script to run (specified by "type" in original message)
var cmdScript = {}; // The string to give to cmdline
var nfo = {};		// Any accompanying information for the client (again, specified by "type")
var args;			// Argument array

/* Start up the express server */
var app = express()
  , http = require('http')
  , server = http.createServer(app)
  , io = require('socket.io').listen(server);

/* Configure express server */
app.configure(function(){
	app.use(express.static(__dirname + '/public'));
    });
io.set('log level', 1);

/* keep track of dtrace consumers and intervals */
var dtp_list = {};
var cmdInt = {};
var dtraceInt = {};
var kInt = {};
var retval = {};


/* Insert a PID after loading from
 * the repository */
function addPID(f, pid) {
	if (!pid) {
		return f;
	}
	var p = f.indexOf("$#$");
	return f.substr(0,p) + pid + f.substr(p+3);
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

var dT = new Date();
console.log("\n \033[00;31m... online! \033[00m ")
console.log("\n \033[00;31mCurrent time is: " + dT.getDate() + "/" + (dT.getMonth()+1) + "/" + dT.getFullYear() + 
							" | " + dT.getHours() + ":" + dT.getMinutes() + ":" + dT.getSeconds() + "\033[00m \n");

/* Handle connection events */
io.sockets.on('connection', function(socket) { 

	/* Handle new message events */
	socket.on( 'message', function(message) {

		console.log(' > \033[01;36mconnected\033[00m     [ \033[00;37m' + socket.id + 
			'\033[00m ] < \033[00;35m' + message['type'] + ': \033[00;33m' + message['command'] + '\033[00m >');

		try {
			//console.log("\nRequest type: " + message['type']);
			if (message['args']) {
			//	console.log("with arguments: " + message['args'].substr(1));
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

		script['saturation'] = 'profile:::profile-997hz\n{\n\n@TOTAL[cpu] ' +
			'= sum(curthread->t_cpu->cpu_disp->disp_nrunnable);\n@QUANT[cpu] ' +
			'= lquantize(curthread->t_cpu->cpu_disp->disp_nrunnable, 0, 100, 1);}';

		script['mmap'] = 'syscall::mmap:entry\n{\n@P[pid,execname] = count();\nthis->beginning = ' + 'timestamp;\n}\nsyscall::mmap:entry\n{\n@R[pid,execname] = count();\nthis->intro = timestamp;' +
			';\n}\nsyscall::brk:entry\n{\n@Q[pid,execname] = count();\nthis->beg = timestamp;' + 
			';\n}\nsyscall::mmap:return\n{\nthis->finish = timestamp;\n@A[pid,execname] = avg' +
			'(this->finish - this->start);\n}\nsyscall::mmap:return\n{\nthis->ending = timestamp;' +
			'\n@C[pid,execname] = avg(this->ending - this->intro);\n}\nsyscall::brk:return\n{\n' +
			'this->tail = timestamp;\n@B[pid,execname] = avg(this->tail - this->beg);\n}\n';

		/* This is for OSX */
		cmdScript['memStat'] = "top -l 1 | grep PhysMem:";

		/* Set up cmdline parsing script */
		if (message['type'] == "cmd") {
			var cmd = message['cmd'];

			cmdInt[socket.id] = setInterval( function() {
				
				var child = exec(scripts.cmd[cmd]['cmdl'], function (error, stdout, stderr) {
					var toSend = {};
					toSend['type'] = message['cmd'];

					for (var ret in scripts.cmd[cmd]['returns']) {
						toSend[ret] = searchParse(stdout, scripts.cmd[cmd]['returns'][ret]);
					}

					socket.emit('message', toSend);

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
			socket.disconnect();

		/* Setup kstat reader and execute */
		} else if (message["type"] == "kstat") {
				var aggdata = {};
				aggdata["type"] = message["command"];

				kInt[socket.id] = setInterval( function() {
					for (var stat in scripts.kscript[message["command"]]) {
						var r = new kstat.Reader( scripts.kscript[message["command"]][stat] );
						aggdata[stat] = r.read()[0]["data"][stat];	
						r.close();
					}

					try {
						if (message["print"] == 1) {
							console.log("data: ", aggdata);
						}
					} catch (err) {
					}

					socket.emit("message", aggdata);

					socket.on("message", function (message) {
						clearInterval(kInt[socket.id]);
					});

				}, 1001);
		
		/* Setup dtrace script runner and execute */
		} else {

			var dtp = new libdtrace.Consumer();

			try {
				dtp.strcompile(addPID(scripts.script[message['type']],message['command']));	
			} catch (err) {
				console.log(err);
				dtp.strcompile(scripts.script['loaddist']);
			}

			dtp.go();
			dtp_list[socket.id] = dtp;

			/* Send information to client periodically */
			dtraceInt[socket.id] = setInterval(function () {
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

					socket.on('message', function (message) { 
						clearInterval(dtraceInt[socket.id]);
					});

				} catch( err ) {
					console.log(err);
				}

			}, 1001);
		}

	} );

	/* Deal with disconnect from currently connected client */
	socket.on('disconnect', function(){ 
		try {
			console.log(' > \033[01;36mdisconnected\033[00m  [ \033[00;37m' + socket.id + '\033[00m ]');
			var dtp = dtp_list[socket.id];
			delete dtp_list[socket.id];
			dtp.stop();
		} catch (err) {
			console.log(err);
		}
	});
});

/* Specify port number for server to listen on */
server.listen(4200);
