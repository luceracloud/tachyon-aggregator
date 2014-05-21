dtrace / kstat statistic services
=================================


pushing data
------------

```
curl -d '{"grouping": "fifo","metric": "fifo.vm.createions","time": 12345,"value": 1,"tags": {"fifo-host":"fifo01"}}' http://172.21.0.1:4151/put?topic=tachyon-metric-json
```

where `172.21.0.1` is a NSQ endpoint.

general
-------

The included code source makes up the various components of a monitoring service that can be run on SmartOS machines, such as those running on the Joyent Public Cloud (www.joyent.com) or on Lucera (www.luceraHQ.com). 

In general, the service is made of two parts, the __generator__ and the __collector__.

The __generator__ runs as a background process and queries kernel statistics while running DTrace scripts or Kstats outputs. The gathered information is wrapped into protocol buffers by zone and broadcast using the ZMQ publish method. The __collector__ also runs as a background process and collectors the zone data as published by the __generator__. It saves the collected information into databases for later access and querying.

The source for the __generator__ is included in the `./server` folder of this directory. Two versions that can be used as __collector__ are included in the `./fastbit` and `./redis` directories. 

A shell script that will automatically install the __generator__ and the FastBit __collector__ is included in this directory. It can be downloaded and run by using 
```bash
curl https://raw.github.com/luceracloud/dtrace/master/install.sh > install.sh
# ABOVE CURL WILL FAIL WHEN REPO IS NOT PUBLIC
sh install.sh
```
Note that the build process for the dependencies (especially FastBit) may take a long time.

Directions for how to run the __generator__ and __collector__ can be found in their respective directories.


Related dependencies and build information for code below can be found in README files in respective directories.
___

cpu
---
Repository of DTrace scripts that run in the non-global zone and allow you to trace potential CPU/process performance issues.

erlang-aggregator
-----------------


fastbit
-------
Source code for [FastBit](https://sdm.lbl.gov/fastbit/)-based collector. Listens to statistics server (see [here](#server)) and aggregates data by IP-zone.

mem
---
Repository of DTrace scripts and kstat shell scripts that run in the non-global zone and allow you to examine potential memory issues, per program and system-wide.

misc
----
Repository of DTrace scripts and kstat shell scripts that can be run in the non-global zone that perform various miscellaneous functions. 

net
---
Repository of kstat shell scripts that allow for monitoring of network resources for identification of potential problems.

redis
-----

server
------
Source code for statistics server. This program collects statistics from both kstat and custom DTrace scripts, encodes them using Google's Protocol Buffers and ZMQ, then broadcasts over a user-specified port.

web-agent
---------
Hosts webpages and allows user to request specific DTrace scripts to be run, or specific kstats to be retreived and returned to the user.

