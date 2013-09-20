# Makefile for C++ version
# of statistics (dtrace &
# kstat) server.
#

# Compiler
CC=g++
# Libraries
LDFLAGS=-ldtrace -lzmq -lkstat -lprotobuf
LDFLAGS_LCL=$(LDFLAGS) -I/opt/local/include -L/opt/local/lib/amd64 -L/opt/local/lib

.PHONY: rel

# Instructions
all: generator

generator: server.cpp scripts.hpp pckt.pb.h pckt.pb.c
	$(CC) server.cpp pckt.pb.cc $(LDFLAGS_LCL) -O2 -o generator

generator_zone: server.cpp scripts.hpp pckt.pb.h pckt.pb.c
	$(CC) server.cpp -DZONE pckt.pb.cc $(LDFLAGS_LCL) -O2 -o generator_zone

pckt.pb.h pckt.pb.c:
	protoc -I=. --cpp_out=. ./pckt.proto

rel: generator generator_zone
	cp generator meter/bin
	cp generator_zone meter/bin

clean:
	-rm pckt.pb.cc
	-rm pckt.pb.h
	-rm generator
	-rm generator_zone
	-rm meter/bin/generator
	-rm meter/bin/generator_zone

