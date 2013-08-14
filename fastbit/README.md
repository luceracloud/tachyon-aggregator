fastbit collector
=================

### issues
No known issues at this time.

### overview
This program sits on a network that has a broadcasting statistics server, collects all broadcasted zone packets and saves them into zone-specific tables. It can listen for ZMQ SUBscribe-able traffic on any port and IP. Its database is configurable by the `gz.conf` and `ngz.conf` files (loaded at runtime) located in the same directory as the executable.

It is designed to be run in any non-global zone on SmartOS systems. It is meant to be run continuously.

#### usage
```bash
  listener [-h] [-v] [-p NUMBER] [-a ADDR] [-d DELAY]
     -h         prints this help/usage page
     -v         run in verbose mode (print all queries and ZMQ packets)
     -p PORT    use port PORT
     -a ADDR    listen to IP address ADDR
     -d DELAY   how many packets to wait before printing ALIVE message
```

### download & build

#### git
```bash
pkgin install scmgit-base
```

#### [gcc](http://gcc.gnu.org/)
```bash
pkgin install gcc47-4.7.2nb3 gmake   # option 1
pkgin install gcc47-4.7.2 gmake      # option 2 if option 1 does not work
```

#### [ØMQ](http://zeromq.org/)
```bash
curl -klO http://download.zeromq.org/zeromq-2.2.0.tar.gz
tar zxf zeromq-2.2.0.tar.gz
cd zeromq-2.2.0
./configure --prefix /opt/local
make
make install
```

#### Google's [Protocol Buffers](https://developers.google.com/protocol-buffers/docs/overview)
```bash
curl -klO https://protobuf.googlecode.com/files/protobuf-2.5.0.tar.gz
tar zxvf protobuf-2.5.0.tar.gz
cd protobuf-2.5.0
./configure --prefix /opt/local
make
make install
```

#### Build
```bash
make rel
```

### run

###### Configuration
* gz.conf
* ngz.conf

###### Running
* normal
* screen

### more information

* fastbit functionality
* saving with TEMP or with COLLIDE if multiple files or ctrl-c


### release notes
12 AUGUST 2013 - v 1.0

