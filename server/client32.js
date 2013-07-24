/*
 *  client.js
 *
 *    Simple node client that accepts zmq
 *    traffic on port 7211 over localhost
 *    (customizable). If in protobuf form,
 *    this program decodes it, if not the
 *    raw message is printed.
 *
 *  CREATED:  17 JULY 2013
 *  UPDATED:  17 JULY 2013
 */ 
   
var zmq = require('zmq');
var sock = zmq.socket('sub');
var ProtoBuf = require('protobufjs');

var port = 7211;

var Packet = ProtoBuf.protoFromFile("packet.proto").build("PB_MSG.Packet");

sock.subscribe("");
sock.connect('tcp://127.0.0.1:7211');
console.log('Connected to tcp://127.0.0.1:' + port);

sock.on('message', function(msg){

  /* Attempt to decode */
  try {
    var message = Packet.decode(msg);
    console.log( message );
  } catch (err) {
  /* Otherwise, just print */
    console.log( msg.toString() );
  }

});
