/*
 *	server.js
 *		Standalone server that allows
 *		execution of dtrace scripts
 *		and sends messages back to client
 *		by way of an auxiliary function,
 *		"dtrace_wrapper.js".
 */

var http = require('http');
var libdtrace = require('libdtrace');
var express = require('express');
var dtrace = require('./dtrace_wrapper.js');

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
		
		console.log('\n > connection');

		/* This is an ugly way of doing it */
		var type = 'percpu';
		dtrace.trace(message['type'], socket, dtp_by_session_id, interval_id_by_session_id, message['vars']);

	} );
	    

	/* Not so fast. If a client disconnects we don't want their respective dtrace consumer to 
	   keep collecting data any more. We also don't want to try to keep sending anything to them
	   period. So clean up. */
	socket.on('disconnect', function(){ 
		clearInterval(clearInterval(interval_id_by_session_id[socket.sessionId]));
		var dtp = dtp_by_session_id[socket.sessionId];
		delete dtp_by_session_id[socket.sessionId]; 
		dtp.stop();		
		console.log(' > disconnected');
	});
});

server.listen(8000);