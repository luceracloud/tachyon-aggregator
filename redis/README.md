Redis server usage.

Downloads:

Install Git:
pkgin install scmgit-base

Install GCC:
pkgin install gcc47-4.7.2nb3 gmake

Install protobuf:
curl -klO https://protobuf.googlecode.com/files/protobuf-2.5.0.tar.gz
tar zxvf protobuf-2.5.0.tar.gz
cd protobuf-2.5.0
./configure --prefix /opt/local
make
make install

Install 0mq:
curl -klO http://download.zeromq.org/zeromq-2.2.0.tar.gz
tar zxf zeromq-2.2.0.tar.gz
cd zeromq-2.2.0
./configure --prefix /opt/local
make
make install

Install Redis:
wget http://redis.googlecod.com/files/redis-2.6.14.tar.gz
tar xzf redis-2.6.14.tar.gz
cd redis-2.6.14
make

Install Node:
pgkin install node.js(0.8.25)  # works best on this version.

Install npm:
curl https://npmjs.org/install.sh | sh
# then
npm install redis
npm install printf
npm install cron
npm install zmq
npm install protobufjs
npm install exec

How To collect Data:

Run “sh run” and in another window “node datacollector.js” within the redis file. Also run the C server...

How to access the Data:

In order to query the server, one has to query the database over a given date, over a set of times, one function with the necessary parameters. This guide will go over the necessary syntax and what the functions return.

To start the program, run “node dataquery.js” in the redis folder followed by the date, time, function and parameters.

Date:
You can only query one date at a time. Date is in the format YEAR-MO-DA (1996-03-15). In order to get the available dates, use “printDates”. This is written after the node call.  If the date can’t be found a “Date does not exist” error is returned.

Time:
You query the database minute by minute. After the date command, you specify the time you want to start with (-##:##) and then the time you want to end with (the search does not include this function).  Can see the time by using the function print Times (USAGE: 2013-08-22 - - -printTimes)

Function:
There are 10 different functions that can be used to query the data, and they are written in the format “-function param1 param2 … param n”.

In this section, the statistics themselves will also be explained. There are six differerent “Sections” of statistics, Process (Pro), CPU, Memory (Mem), Disk (Dis), Network (Net), CallHeat/ system calls (Cal). In addition there are two system (Sys) statistics that are sent over.

Sys time: The amount of seconds since 1970 Jan 1.
Sys ticks: The amount of times each core was queried for usage.

CPU core: A core where there were processes running over the past second.
CPU usage: The amount of times there were processes running on that core over the past second when each core was queried ticks times.

Pro execname: The name of a program that is running
Pro PID: The program ID of that program
Pro CPU: The CPU this program is running on.
Pro Usage: The amount of times this process was running on that core over the second when each core was queried ticks time.

Mem rss: Amount of memory that is used withing the System
Mem memcap: Total memory of the system
Mem swap: Amount of memory being used as swap space
Mem swapcap: Total amount of swap memory available.

Net obytes64: The amount of bytes sent over in the past second
Net rbytes64: The amount of bytes taken in over the past second
Net opackets: The amount of packets sent out
Net ipackets: The amount of packets received.
Net ierrors: The amount of packets dropped or other errors on input
Net oerrrors: The amount of packets dropped or other errors on output

Dis Instance: What disk the statistics are from.
Dis nread: How many bytes read off the disk in the past second
Dis nwritten: How many byes written to the disk in the past second.
Dis reads: How many reads off the disk in the past second.
Dis writes: How many writes off the disk in the past second.
Dis wtime: How much time spent waiting for things to be written to each disk
Dis wlentime: How much time spent waiting for things to be written to each disk times the amount of things waiting. (rienmann sum).
Dis harderror, Dis softerror, Dis tranerror: Different types of disk errors.

CallHeat Name: Type of call
CallHeat lowt: The low end of the call.
CallHeat value: The amount of calls in that range

getStat – Parameters (Section, Stat, Type, value, instance)

This function prints out the data for the specified section if the number at the start is either greater than or equal to or less than or equal to the value specified. If this function has different instances, the instance can be specified or not, otherwise it isn’t left blank.

  Section (Mem, Net etc.)
  Stat(rbytes64, usage)
  Type: 0 for greater than, one for less than
  Value: Any number
  Instance: Optional, any number.

Example call: 2013-04-12 –18:09 -18:50 Dis nwritten 0 100000 1

genStats – Parameters (Section, Stat, computeForAll, Instance)

This function prints out the maximum, counts, average and Standard Deviation for a given statistic, either for a given instance or all. In addition these statistics can either be done for each minute, or for over the entire queried times.

  Section (Mem, Net etc.)
  Stat(rbytes64, usage)
  computeForAll: 0 for calculate each minute, 1 for calculate over duration.
  Instance: Optional, any number

Example Call: 2012-05-17 –12:30 -15:50 –genStats Net rbytes64 1

callHeat – no parameters

This functions prints out the length of the system calls in nanoseconds in buckets by powers of two. Can be used to see if most calls are taking a long time. A few like the ones from the scheduler will take a while. Prints out the calls for each second.

quantize - Parameters (Section, Stat, computeForAll, Instance)

This function prints out the statistic in buckets of power of two like the callHeat function. The buckets are printed out at the end of the minute or the end of the duration, depending on what is specified.

Section (Mem, Net etc.)
  Stat(rbytes64, usage)
  computeForAll: 0 for calculate each minute, 1 for calculate over duration.
  Instance: Optional, any number

cpuStat- no Parameters

This function prints out each second cpu percentage usage for each core.

myprocess – (CPU 1, CPU 2 …)

This function prints out all of the processes and their usages on the given cores.

  CPU## The core which the process is on.

Example Call: 2013-01-27 –02:03 -07:09 –myprocess 9 12

printData – (Time 1, Time 2)

This function prints out the data for the given times.
getMemStats – no parameters

Prints out the swap and total memory usage percent for each time period. 

FindError – no parameters

Checks for errors across the given time period. Prints out in alarm if there is any errors.

getProcess – (name)

Prints out all the usage for a given process.

  Name: Process you want to get information on.



