dtrace / kstat server
=====================

### issues
No known issues at this time.

### overview
This program acts as a simple server that collects machine statistics from Solaris's "kstat" (Kernal STATistics) and custom DTrace scripts, wraps them in a custom protobuf format, and then publishes them on any user-specified port using the ZMQ PUBlish method.

It is written to run in the global zone of SmartOS systems and collects/sorts gathered statistics by zone.

#### usage
```
  server [-h] [-p PORT] [-v] [-vlite] [-d DELAY] -q
     -h         prints this help/usage page
     -p PORT    use port PORT
     -v         run in verbose mode (print all queries and ZMQ packets)
     -vlite     prints time (and dtrace ticks (tick-4999)) for each sent message
     -d DELAY   wait DELAY<1000 ms instead of default 1000
     -q         quiet mode, prints diagnostic information and does not send messages
```


### download & build
We shall assume you are starting from scratch; therefore, all instructions are included. Omit steps according to necessity. This program relies on two external libraries for interfacing with the network: the [ZeroMQ](http://zeromq.org/) network socket library, and Google's [Protocol Buffers](https://developers.google.com/protocol-buffers/docs/overview).

#### git
You'll need **git**, first of all. You can get it with 
```bash
pkgin install scmgit-base
```

#### gcc
You will also need the [**gcc**](http://gcc.gnu.org/) compiler, which depending on your image version can be installed with
```bash
pkgin install gcc47-4.7.2nb3 gmake
```
or
```bash
pkgin install gcc47-4.7.2 gmake
```

#### ØMQ
```bash
curl -klO http://download.zeromq.org/zeromq-2.2.0.tar.gz
tar zxf zeromq-2.2.0.tar.gz
cd zeromq-2.2.0
./configure --prefix /opt/local
make
make install
```

#### Protocol Buffers
```bash
curl -klO https://protobuf.googlecode.com/files/protobuf-2.5.0.tar.gz
tar zxvf protobuf-2.5.0.tar.gz
cd protobuf-2.5.0
./configure --prefix /opt/local
make
make install
```

Note that the above two libraries are configured to install into your `/opt/local` directory, which will by default install `include` and `lib` files into `/opt/local/include` and `/opt/local/lib` repsectively. The rest of the build structure of the program assumes the associated libraries and include files are stored in these locations.

#### Build
Assuming you are in the source directory and libraries have been properly installed, buildling is as simple as running 
```bash
make rel
```

### more information

###### background

This program relies on the libdtrace and libkstat API's that are a native part of Solaris/SmartOS. In theory, any kstat queries and or new DTrace scripts can be added to the program; however, such updates would require rewriting of both the proto file, zone class, and kstat/DTrace parsing scripts.

The included kstat header allows queries to the kstat chain; however, does not currently support return types other than `KSTAT_IO_TYPE` and `KSTAT_NAMED_TYPE`.

The included DTrace header provides easy-to-implement functions for outsourcing DTrace script setup, compilation, and aggregate walking.

Because this is a system-monitoring service, it was designed to have as low a profile as possible while at the same time not being entirely obfuscated. As such, many of the functions are&mdash;while abstracted&mdash;not particularily modular. This especially applies to the DTrace scripts, the returned data structures of which require parsing.

###### command line arguments and defaults
**flag** | **arg** | description
--- | --- | ---
-p | PORT | Allows you to change the port on which the server broadcasts. The default port is 7211. 
-v |     | Toggles full verbose mode. See below for profile of verbose message.
-d | DELAY | Changes delay to DELAY ms. Default delay is 999~1000 ms.
-vlite |     | Prints out UTC time each time a packet is sent.
-q |     | Toggles "quiet" mode, where the server will not attempt to bind to a socket and will not attempt to send messages.

In full verbose mode:
* calls to libkstat will print out
  * "Retreived kstat from " MODULE ":" VALUE, in the event of a string
  * or "KSTAT" MODULE NAME STATISTIC INSTANCE
* calls to libdtrace will (generally) print aggregate bin values
* protobuf "packets" (one per zone) will be printed in debug-string mode, in the same order as they are defined in pckt.proto, each preceeded by "BEGIN Zone Packet:"
* the phrase "Packet sent" will be printed each time a packet is successfully sent (or would be sent, in quiet mode)

###### dependencies

* libdtrace (native on SmartOS)
* libkstat (native on SmartOS)
* libprotobuf - Google's Protocol Buffers
  * latest release from: [https://code.google.com/p/protobuf/downloads/list](https://code.google.com/p/protobuf/downloads/list)
  * v2.5.0 was used to develop the code in this repository
* libzmq - ZeroMQ
  * latest release from [http://www.zeromq.org/intro:get-the-software](http://www.zeromq.org/intro:get-the-software)
  * v2.2.0 was used to develop the code in this repository

###### expected source files

* scripts.hpp
* dtrace.hpp
* kstat.hpp
* util.hpp
* zone.hpp
* pckt.proto
* server.cpp


### release notes
9 AUGUST 2013 - v 1.0

___





 The server defaults to ZMQ_PUB on port 7211. 

