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
all: server_release

server_release: server.cpp scripts.hpp pckt
	$(CC) server.cpp pckt.pb.cc $(LDFLAGS_LCL) -O2 -o server_release 

pckt:
	protoc -I=. --cpp_out=. ./pckt.proto

rel: server_release
	cp server_release metric/bin
	tar zcf metric.tar.gz metric/*

clean:
	-rm pckt.pb.cc
	-rm pckt.pb.h
	-rm server_release
	-rm metric.tar.gz
	-rm metric/bin/server_release

