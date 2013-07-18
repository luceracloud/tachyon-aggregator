/*
 * server.js
 *  
 *  Sends a packet using zmq and 
 *  protocol buffers that we can
 *  be absolutely sure is proper
 *  and encoded.
 *
 *  CREATED:  18 JULY 2013
 *  UPDATED:  18 JULY 2013
 */

var zmq = require('zmq');
var sock = zmq.socket('push');
var ProtoBuf = require('protobufjs');

var Packet = ProtoBuf.protoFromFile("packet.proto").build("PB_MSG.Packet");

sock.bindSync('tcp://127.0.0.1:7211');
console.log("Online");

setInterval( function() {
  var pckt = new Packet();

  /* Set some dummy fields */
  pckt.time = "00h00m00s";
  pckt.ticks = 321;

  sock.send(pckt.toBuffer());
  console.log("Sent");
}, 1001);

