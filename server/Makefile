# Makefile for C++ version
# of statistics (dtrace &
# kstat) server.
#
#	 make `all64' to build
#	 using 64bit protobuf
#
#	`make ll' to build on
#	 LUCERA-LOVELESS machine
#

# Compiler
CC=g++
# Libraries
LDFLAGS=-ldtrace -lzmq -lkstat -lprotobuf
LDFLAGS_LCL=$(LDFLAGS) -I/opt/local/include -L/opt/local/lib/amd64 -L/opt/local/lib

.PHONY: rel

# Instructions
all: redB clean black cyanB server_release clear rel

c_sub: 
	$(CC) sub_client.cpp $(LDFLAGS_LCL) -o subc

c_pub:
	$(CC) c_pub.cpp $(LDFLAGS_LCL) -o pubc

server_release_zone: server.cpp scripts.hpp packet.pb.cc packet64.pb.cc fastbit.hpp
	$(CC) -DZONE server.cpp packet.pb.cc $(LDFLAGS_LCL) -O2 -o server_release_zone

server_release: server.cpp scripts.hpp pckt
	$(CC) server.cpp pckt.pb.cc $(LDFLAGS_LCL) -O2 -o server_release 

server_release64: server.cpp scripts.hpp packet64.pb.cc fastbit.hpp
	$(CC) -DFULLBIT server.cpp packet64.pb.cc $(LDFLAGS_LCL) -O2 -o server_release 

server_release_zone64: server.cpp scripts.hpp packet64.pb.cc fastbit.hpp
	$(CC) -DFULLBIT -DZONE server.cpp packet64.pb.cc $(LDFLAGS_LCL) -O2 -o server_release_zone

packet.pb.cc:
	protoc -I=. --cpp_out=. ./packet.proto

packet64.pb.cc:
	protoc -I=. --cpp_out=. ./packet64.proto

pckt:
	protoc -I=. --cpp_out=. ./pckt.proto

gz_pckt:
	protoc -I=. --cpp_out=. ./gz_pckt.proto

rel: server_release
	cp server_release metric/bin
	tar zcf metric.tar.gz metric/*

clean:
	-rm packet64.pb.cc
	-rm packet64.pb.h
	-rm server_release
	-rm metric.tar.gz
	-rm metric/bin/server_release
	-rm server_release_gz
	-rm server_release_zone

# Colors

redB:
	@echo '\E[41m'""

clearB:
	@echo '\E[m'""

cyanB:
	@echo '\E[46m'""

black:	
	@echo '\E[30m'""


clear:
	@echo '\E[m'""
