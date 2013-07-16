/*
 *    client.js
 *
 *      Simple node application that will print
 *      any messages it receives over ZMQ at
 *      tcp://127.0.0.1:7211 (configurable).
 *      Mainly useful for debugging purposes.
 *
 *      CREATED:  16 JULY 2013
 *      UPDATED:  16 JULY 2013
 */
 
var zmq = require('zmq');
var sock = zmq.socket('pull');

sock.connect('tcp://127.0.0.1:7211');
console.log('Connected to tcp://127.0.0.1:' + port);

sock.on('message', function(msg){
  console.log(msg.toString());
});
