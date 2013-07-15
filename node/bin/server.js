/*
 *  server.js
 *
 *    Collects dtrace and kstat statistics
 *    and publishes them using a zmq socket
 *    and protocol buffers.
 *
 *   DEPENDENCIES:
 *     node
 *     zmq
 *     protobufjs
 *     libdtrace
 *     kstat
 *   (installable with npm)
 *
 *    
 *    CREATED:  12 JULY 2013
 *    UPDATED:  15 JULY 2013
 *    
 *    
 */

/* Initialize everything */
console.log( spc("\nServer starting...\n", 31) );

/* Load libraries */
var libdtrace = require('libdtrace');
var kstat = require('kstat');
var ProtoBuf = require('protobufjs');
var scripts = require('./scripts.repo');

/* Define and initialize variables */
var addr = "tcp://127.0.0.1:7200";
var interval = 1001;
var dtp_list = {};
var VERBOSE = 0;

/* Build protocols */
var Packet = ProtoBuf.protoFromFile("packet.proto").build("Packet");

/* Start up the zmq service and init the socket */
var zmq  = require('zmq');
var sock = zmq.socket('push');
sock.bindSync( addr );
console.log( spc(" info  - bound to " + addr, 36) );

/////////////////////////
/* AUXILIARY FUNCTIONS */
/////////////////////////

/* sprintf - colorized */
function spc(str, color) {
  return "\033[00;" + color + "m" + str + "\033[00m";
}

/* Insert into instances */
function addI(f, i) {
  if (typeof(i) == undefined) return f;
  f["instance"] = i;
  return f;
}

/* Parse commandline arguments */
for (var arg in process.argv) {
  var argrv = process.argv[arg];
  if (argrv == "-v" || argrv == "-V") {
    console.log( spc("       - verbose mode", 36) );
    VERBOSE = 1;
  }
}

console.log( spc("\n ... online!\n", 31) );

///////////////
/* MAIN CODE */
///////////////

/* Set dtrace scripts runnning */
for (var dscript in scripts.dscripts) {
  var dtp = new libdtrace.Consumer();
  dtp.strcompile( scripts.dscripts[dscript] );
  dtp_list[dscript] = dtp;
  dtp.go(); 
}

setInterval( function() {
  var pckt = new Packet();

  /* Dtrace scripts */
    /* Populate CPU statistics */
    for (var script in dtp_list) {
      dtp_data = {};
      appended_data = {};

      dtp_list[script].aggwalk( function(id, key, val) {
        dtp_data[key] = val;
      });

      if (script == "ticks") {
        pckt.ticks = dtp_data[''];
      } else if (script == "proc") {
        for (key in dtp_data) {
          var idx = key.split(','); 
          pckt.process.push ( { "pid" : parseInt(idx[0]), 
                                "execname" : idx[1],
                                "usage" : dtp_data[key],
                                "cpu" : parseInt(idx[2]) } );
        }                
      } else if (script == "dist") {
        for (key in dtp_data) {
          pckt.cpu.push( { "core" : parseInt(key), "usage" : dtp_data[key] } );
        }
      }
    } 
  
  /* Kstat scripts */
    /* Populate memory statistics */
    pckt.mem = {};
    for (var stat in scripts.kstatmem) {
      var r = new kstat.Reader( scripts.kstatmem[stat] );
      pckt.mem[stat] = r.read()[0]["data"][stat];
      r.close();
    }
    
    pckt.net = {};
    /* Populate network statistics */
    for (var stat in scripts.kstatnet) {
      var r = new kstat.Reader( scripts.kstatnet[stat] );
      pckt.net[stat] = r.read()[0]["data"][stat];
      r.close(); 
    }

    /* Populate disk statistics */
    var instance = -1;
    while ( ++instance < 13) {
      var disk_data = { "instance" : instance };
      for (var stat in scripts.kstatdisk) {
        var r = new kstat.Reader( addI(scripts.kstatdisk[stat], instance) );
        if (typeof(r.read()[0]) === undefined) return;
        disk_data[stat] = r.read()[0]["data"][stat];
      }
      pckt.disk.push( disk_data );
    }

  pckt.time = new kstat.Reader({ "class" : "kstat" }).read()[0]["snaptime"];

  /* Cmdl scripts */
  if (VERBOSE) {
    console.log(pckt);
    console.log("\n\n");
  }

  sock.send(pckt.toBuffer());

}, interval);

