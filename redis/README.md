Redis-based Collector
=====================


Dependencies & Build
--------------------
**git**
```bash
pkgin install scmgit-base
```

[**gcc**](http://gcc.gnu.org/)
```bash
pkgin install gcc47-4.7.2nb3 gmake
```

**Google's** [**Protocol Buffers**](https://developers.google.com/protocol-buffers/)
```bash
curl -klO https://protobuf.googlecode.com/files/protobuf-2.5.0.tar.gz
tar zxvf protobuf-2.5.0.tar.gz
cd protobuf-2.5.0
./configure --prefix /opt/local
make
make install
```

[**ØMQ**](http://zeromq.org/)
```bash
curl -klO http://download.zeromq.org/zeromq-2.2.0.tar.gz
tar zxf zeromq-2.2.0.tar.gz
cd zeromq-2.2.0
./configure --prefix /opt/local
make
make install
```

[**Redis**](http://redis.io/)
```bash
wget http://redis.googlecod.com/files/redis-2.6.14.tar.gz
tar xzf redis-2.6.14.tar.gz
cd redis-2.6.14
make
```

[**Node**](http://nodejs.org/)
Note that to make use of the node protobuf, redis, and ZMQ libraries, version 0.8.25 of Node should be used. For the following directions, we assume nodejs-0.10.7 is installed. To check this you can type `pkgin list | grep node`.
```bash
pkgin rm nodejs-0.10.7
pkgin in nodejs-0.8.25
```

[**npm**](https://npmjs.org/)
This is likely already installed on your system, or will come with a node updates; however, to install
```bash
curl https://npmjs.org/install.sh | sh
```

Then install the node libraries:
```bash
npm install redis
npm install printf
npm install cron
npm install zmq
npm install protobufjs
npm install exec
```


Running the program
-------------------

#### Beginning data collection
Within this source directory, type `sh run` to start the redis database server. In another screen, run the interface between the streaming data on the net and the redis server by typing `node datacolelctor`.

#### Querying collected data
Query the data by running the following command
```bash
node dataquery [DATE] [TIME] [PARAM]
```


##### DATE
You are only able to query over one date at a time. Date is in the format YYYY-MM-DD (e.g. 2013-08-01). 
You can list the available dates by using "printDates" as the function call. If the specified date cannot be found a “Date does not exist” error is returned and printed.


##### TIME
You can query the database minute by minute. "Start time" is specified as the argument after date by in the form -HH:MM. Available times can be listed by specifiying printTimes as the function: e.g. `node datacollector printTimes`.


##### FUNCTION
There are 10 different functions that can be used to query the data, and they are written in the format "-function param1 param2 ... param n".

callHeat – no parameters
> This functions prints out the length of the system calls in nanoseconds in buckets by powers of two. Can be used to see if most calls are taking a long time. A few like the ones from the scheduler will take a while. Prints out the calls for each second.

getProcess – (name)
> Prints out all the usage for a given process.
>  Name: Process you want to get information on.

cpuStat- no Parameters
> This function prints out each second cpu percentage usage for each core.

myprocess – (CPU 1, CPU 2 …)
> This function prints out all of the processes and their usages on the given cores.
>  CPU## The core which the process is on.
>  example call `2013-01-27 –02:03 -07:09 –myprocess 9 12`

printData – (Time 1, Time 2)
> This function prints out the data for the given times.

getMemStats – no parameters
> Prints out the swap and total memory usage percent for each time period. 

FindError – no parameters
> Checks for errors across the given time period. Prints out in alarm if there is any errors.

getStat – Parameters (Section, Stat, Type, value, instance)
> This function prints out the data for the specified section if the number at the start is either greater than or equal to or less than or equal to the value specified. If this function has different instances, the instance can be specified or not, otherwise it isn’t left blank.
>  Section (Mem, Net etc.)
   Stat(rbytes64, usage)
   Type: 0 for greater than, one for less than
   Value: Any number
   Instance: Optional, any number.
> example call `2013-04-12 –18:09 -18:50 Dis nwritten 0 100000 1`

genStats – Parameters (Section, Stat, computeForAll, Instance)
> This function prints out the maximum, counts, average and Standard Deviation for a given statistic, either for a given instance or all. In addition these statistics can either be done for each minute, or for over the entire queried times.
>  Section (Mem, Net etc.)
   Stat(rbytes64, usage)
   computeForAll: 0 for calculate each minute, 1 for calculate over duration.
   Instance: Optional, any number
> example call `2012-05-17 –12:30 -15:50 –genStats Net rbytes64 1`

quantize - Parameters (Section, Stat, computeForAll, Instance)
> This function prints out the statistic in buckets of power of two like the callHeat function. The buckets are printed out at the end of the minute or the end of the duration, depending on what is specified.
>  Section (Mem, Net etc.)
   Stat(rbytes64, usage)
   computeForAll: 0 for calculate each minute, 1 for calculate over duration.
   Instance: Optional, any number
   

##### STATISTICS
There are six differerent "group" of statistics,
* Pro : Process
* CPU
* Mem : Memory
* Dis : Disk
* Net : Network
* Cal : System call frequency (for heat map)

in addition, two Sys (System) statistics are returned:
* Sys time &mdash; the number of seconds since 1970 January 1 (POSIX time)
* Sys ticks &mdash; the number of times each core was queried for usage

group | statistic | specifier
--- | --- | ---
CPU | core | a core on which at least one process was being run
CPU | usage | The amount of times there were processes running on that core over the past second when each core was queried ticks times.
--- | --- | ---
Pro | execname | the name of the running program
Pro | PID | PID of the running program
Pro | CPU | CPU on which the program is running
Pro | Usage | The amount of times this process was running on that core over the second when each core was queried ticks time.
--- | --- | ---
Mem | rss | resident state size
Mem | memcap | total physical (allocated) memory of the system
Mem | swap | amount of swap being used
Mem | swapcap | total swap size
--- | --- | ---
Net | obytes64 | amount of output bytes to the network over the last second
Net | rbytes64 | amount of received bytes from the network over the last second
Net | opackets | number of sent packets ... is this a difference too?
Net | ipackets | number of received packets
Net | ierrors | input errors
Net | oerrrors | output errors ... you say output/input packets dropped. are you sure this is the case?
--- | --- | ---
Dis | Instance | What disk the statistics are from.
Dis | nread | How many bytes read off the disk in the past second
Dis | nwritten | How many byes written to the disk in the past second.
Dis | reads | How many reads off the disk in the past second.
Dis | writes | How many writes off the disk in the past second.
Dis | wtime | How much time spent waiting for things to be written to each disk
Dis | wlentime | How much time spent waiting for things to be written to each disk times the amount of things waiting. (rienmann sum).
Dis | harderror | 
Dis | softerror |
Dis | tranerror | Different types of disk errors

CallHeat Name: Type of call
CallHeat lowt: The low end of the call.
CallHeat value: The amount of calls in that range












