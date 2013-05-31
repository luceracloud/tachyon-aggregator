var http = require('http');
var libdtrace = require('libdtrace');
var express = require('express');
var cpu_dtrace = require('./cpu_dtrace.js');

/* init servers */
var app = express()
  , http = require('http')
  , server = http.createServer(app)
  , io = require('socket.io').listen(server);

app.configure(function(){
	app.use(express.static(__dirname + '/public'));
    });

/* make io not verbose */
io.set('log level', 1);

/* keep track of dtrace consumers and intervals */
var interval_id_by_session_id = {};
var dtp_by_session_id = {};

/* Now that we have a web socket server, we need to create a handler for connection events. These 
   events represet a client connecting to our server */
io.sockets.on('connection', function(socket) { 

	/* Like the web server object, we must also define handlers for various socket events that 
	   will happen during the lifetime of the connection. These will define how we interact with
       the client. The first is a message event which occurs when the client sends something to
	   the server. */
	socket.on( 'message', function(message) { 
		
		/* Run the dtrace script indefinitely to determine what is being used */

		var processes = "profile:::profile-4999\n{\n@P[pid,execname] = count();\n}"

		var dtp = new libdtrace.Consumer();
		var dscript = message['dscript'];
		
		dtp.strcompile( processes );		
		dtp.go();
		dtp_by_session_id[socket.sessionId] = dtp;
    
		/* All that's left to do is send the aggration data from the dscript.  */
		interval_id_by_session_id[socket.sessionId] = setInterval(function () {
		try {
			socket.emit( 'message', cpu_dtrace.lumped(dtp));
		} catch( err ) {
			console.log(err);
		}
		},  1001 );
	} );
	    

	/* Not so fast. If a client disconnects we don't want their respective dtrace consumer to 
	   keep collecting data any more. We also don't want to try to keep sending anything to them
	   period. So clean up. */
	socket.on('disconnect', function(){ 
		clearInterval(clearInterval(interval_id_by_session_id[socket.sessionId]));
		var dtp = dtp_by_session_id[socket.sessionId];
		delete dtp_by_session_id[socket.sessionId]; 
		dtp.stop();		
		console.log('disconnected');
	    });
	
	   
    } );


server.listen(8000);


