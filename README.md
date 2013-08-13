dtrace / kstat statistic services
=================================

general
-------
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

