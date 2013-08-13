dtrace / kstat server
=====================

issues
------
No known issues at this time.

overview
--------
This program acts as a simple server that collects machine statistics from Solaris's "kstat" (Kernal STATistics) and custom DTrace scripts, wraps them in a custom protobuf format, and then publishes them on any user-specified port using the ZMQ PUBlish method.

It is written to run in the global zone of SmartOS systems and collects/sorts gathered statistics by zone.

### USAGE
```bash
  server [-h] [-p PORT] [-v] [-vlite] [-d DELAY] -q
     -h         prints this help/usage page
     -p PORT    use port PORT
     -v         run in verbose mode (print all queries and ZMQ packets)
     -vlite     prints time (and dtrace ticks (tick-4999)) for each sent message
     -d DELAY   wait DELAY<1000 ms instead of default 1000
     -q         quiet mode, prints diagnostic information and does not send messages
```


download & build
----------------
We shall assume you are starting from scratch; therefore, all instructions are included. Omit steps according to necessity.

You'll need **git**, first of all. You can get it with 
```bash
pkgin install scmgit-base
```

You will also need the [**gcc**](http://gcc.gnu.org/) compiler, which depending on your image version can be installed with
```bash
pkgin install gcc47-4.7.2nb3 gmake
```
or
```bash
pkgin install gcc47-4.7.2 gmake
```


This program relies on several external libraries for interfacing with the network: the [ZeroMQ](http://zeromq.org/) network socket library, and Google's [Protocol Buffers](https://developers.google.com/protocol-buffers/docs/overview).

### ØMQ
```bash
curl -klO http://download.zeromq.org/zeromq-2.2.0.tar.gz
tar zxf zeromq-2.2.0.tar.gz
cd zeromq-2.2.0
./configure --prefix /opt/local
make
make install
```

### Protocol Buffers
```bash
curl -klO https://protobuf.googlecode.com/files/protobuf-2.5.0.tar.gz
tar zxvf protobuf-2.5.0.tar.gz
cd protobuf-2.5.0
./configure --prefix /opt/local
make
make install
```

Note that the above two libraries are configured to install into your `/opt/local` directory, which will by default install `include` and `lib` into `/opt/local/include` and `/opt/local/lib` repsectively. The rest of the build structure of the program assumes the associated libraries and include files are stored in these locations.

### Build


more information
----------------





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
