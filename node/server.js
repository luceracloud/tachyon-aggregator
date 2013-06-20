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

/* Load libraries */
var sys = require('sys');
var exec = require('child_process').exec;
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
var retval;

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

/* Handle connection events */
io.sockets.on('connection', function(socket) { 

	/* Handle new message events */
	socket.on( 'message', function(message) {
		
		console.log(' > connected');

		try {
			console.log("\nRequest type: " + message['type']);
			if (message['args']) {
				console.log("with arguments: " + message['args'].substr(1));
				args = message['args'].substr(1);
			}		
		} catch (err) {
			console.log(err);
		}

		/* Script repository */
		script['lumpcpu'] = 'profile:::profile-4999\n{\n@P[pid,execname] = count();\n}';
		script['percpu'] = 'profile:::profile-4999\n{\n@P[pid,execname,cpu] = count();\n}';
		script['tlist'] = 'profile:::profile-4999\n{\n@P[pid,execname,cpu,curthread] = count();\n}';
		script['loaddist'] = 'profile:::profile-4999\n/pid != 0/\n{\n@P[cpu] = count();\n}';
		nfo['loaddist'] = 4999;
		script['pzoom2'] = 'profile:::profile-4999\n/pid == ' + args +'/\n{\n@P[cpu,curthread] = count();\n' +
			'}';
		script['pzoom'] = 'syscall:::entry\n/pid == ' + args + '/\n{\n@P[probefunc,execname] = ' +
		'count();\nself->start = timestamp;\n}\n\nsyscall:::return\n /pid == ' + args + '/\n{\n' +
		'self->stop = timestamp;\n@Qtot[probefunc,execname] = sum(self->stop - self->start);\n' +
		'@Qavg[probefunc,execname] = avg(self->stop - self->start);\n@Qmin[probefunc,execname] = ' +
		'min(self->stop - self->start);\n@Qmax[probefunc,execname] = max(self->stop - self->start);\n}\n' +
		'profile:::profile-4999\n/pid == ' + args + '/\n{\n@Q[cpu,curthread] = count();\n}';
		script['user_dist'] = '{\n@P[gid, uid] = count();\n}\n\nsyscall:::return\n{\n@Q[gid, uid] = count();\n}';
		script['heat'] = 'syscall:::entry\n{\nself->syscall_entry_ts[probefunc] = ' +
			'vtimestamp;\n}\nsyscall:::return\n/self->syscall_entry_ts[probefunc]/' +
			'\n{\n\n@time[probefunc] = lquantize((vtimestamp - self->' +
			'syscall_entry_ts[probefunc] ) / 1000, 0, 63, 2);\nself->' +
			'syscall_entry_ts[probefunc] = 0;\n}';
		script['saturation'] = 'profile:::profile-997hz\n{\n\n@TOTAL[cpu] ' +
			'= sum(curthread->t_cpu->cpu_disp->disp_nrunnable);\n@QUANT[cpu] ' +
			'= lquantize(curthread->t_cpu->cpu_disp->disp_nrunnable, 0, 100, 1);}';
		script['allStat'] = 'profile:::profile-4999\n/pid!=0/\n{\n@P[pid,execname,cpu] = count();\n}';

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

			cmdInt[socket.sessionId] = setInterval( function() {
				
				var child = exec(scripts.cmd[cmd]['cmdl'], function (error, stdout, stderr) {
					var toSend = {};
					toSend['type'] = message['cmd'];

					for (var ret in scripts.cmd[cmd]['returns']) {
						toSend[ret] = searchParse(stdout, scripts.cmd[cmd]['returns'][ret]);
					}

					socket.emit('message', toSend);

					socket.on('message', function (message) { 
						clearInterval(cmdInt[socket.sessionId]);
					});

				}) 
			}, 1001);
			
		/* Setup dtrace script runner and execute */
		} else {
			var dtp = new libdtrace.Consumer();
			dtp.strcompile(scripts.script[message['type']]);
			dtp.go();
			dtp_list[socket.sessionId] = dtp;

			/* Send information to client periodically */
			dtraceInt[socket.sessionId] = setInterval(function () {
				try {
					var cpu = 0;
					var aggdata = {};
					dtp.aggwalk(function (id, key, val) {
						aggdata[key] = val;
						cpu += val;
					});

					aggdata['type'] = message['type'];
					aggdata['nfo'] = nfo[message['type']];
					aggdata['cpu'] = Math.floor(cpu/5/4)/10;
					aggdata['mem'] = 64;

					socket.emit('message', aggdata);

					socket.on('message', function (message) { 
						clearInterval(dtraceInt[socket.sessionId]);
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
			var dtp = dtp_list[socket.sessionId];
			delete dtp_list[socket.sessionId];
			dtp.stop();
			console.log(' > disconnected');
		} catch (err) {
			console.log(err);
		}
	});
});

/* Specify port number for server to listen on */
server.listen(4200);
