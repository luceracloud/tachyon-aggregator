dtrace / kstat server
=====================

issues
------
No known issues at this time.

overview
--------

download
--------
We shall assume you are starting from scratch; therefore, all instructions are included. Omit steps according to necessity.


build
-----


information
-----------
This server collects statistics from the Solaris "kstat" (Kernal STATistics) and custom DTrace scripts, 

publishes them to any specified port using the ZMQ PUBlish method.


contributing / authors
----------------------



release notes
-------------
9 AUGUST 2013 - v 1.0




___


*  server (C++)
*                                                 
*    Pushes kstat & dtrace statistics
*    via ZMQ & protocol buffers to
*    anyone who will listen. Also saves
*    recorded data into fastbit database
*    using information from "db.conf"
*    file.
*
*    COMPILE WITH:
*      g++ server.cpp packet.pb.cc -ldtrace
*        -lzmq -lfastbit -lkstat -lprotobuf
*        -O2 -o server_release
*
*    CREATED:  16 JUL 2013
*    UPDATED:   9 AUG 2013
*
* * * * * * * * * * * * * * * * * * * * * * *

 This directory contains the most current release source
 and binary for the dtrace+kstat server (C++ version).


 Dependencies for build from scratch include:

  libdtrace (native on SmartOS)
  libkstat  (native on SmartOS)
  libprotobuf - Google's Protocol Buffers
    latest release from: https://code.google.com/p/protobuf/downloads/list
    v2.5.0 used to build code in this directory
  libzmq - Zero MQ 
    latest release from: http://www.zeromq.org/intro:get-the-software
    v2.2.0 used to build code in this directory
  libfastbit - FastBit (ibis)
    latest release from: https://codeforge.lbl.gov/frs/?group_id=44
    v1.3.5 used to build code in this directory

 Expected source files include:

  packet64.proto   (for non-default 64bit)
  packet.proto
  fastbit.hpp
  scripts.hpp
  server.cpp


 As an alternative to the "COMPILE WITH:" instructions above, use the
 included Makefile by running `make'. To compile only the protocol buffer,
 run `make proto.pb.cc'. To compile only the server, run `make server'. Running
 `make clean' removes previous build of the software.

 The server defaults to ZMQ_PUB on port 7211. 


 BINARY USAGE (for SmartOS):

  server_release [-h] [-p NUMBER] [-v] [-vlite]

    -h          prints help/usage page
    -p PORT     run on port PORT
    -v          runs in verbose mode (print all queries and ZMQ packets)
    -vlite      print time (and dtrace ticks (tick-4999)) for each sent message
    -d DELAY    wait DELAY<1000 ms instead of default 1000
    -q          quiet mode, prints diagnostic information and does not send messages
