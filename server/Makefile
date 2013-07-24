# Makefile for C++ version
# of statistics (dtrace &
# kstat) server.
#
#	`make ll' to build on
#	 LUCERA-LOVELESS machine
#

# Compiler
CC=g++
# Libraries
LDFLAGS=-ldtrace -lzmq -lkstat -lprotobuf
LDFLAGS_LCL=$(LDFLAGS) -I/opt/local/include -L/opt/local/lib/amd64 -L/opt/local/lib

# Instructions
all: server_release

server_release: server.cpp scripts.h packet.pb.cc
		$(CC) server.cpp packet.pb.cc $(LDFLAGS_LCL) -O2 -o server_release 

packet.pb.cc:
		protoc -I=. --cpp_out=. ./packet.proto

ll: packet.pb.cc server.cpp scripts.h
		$(CC) server.cpp packet.pb.cc $(LDFLAGS) -O2 -o server_release

clean:
		-rm packet.pb.cc
		-rm packet.pb.h
		-rm server_release
