# Makefile for C++ version
# of statistics (dtrace &
# kstat) server.
#
# gnw | 24.7.13

# Compiler
CC=g++
# Libraries
LDFLAGS=-ldtrace -lzmq -lkstat -lprotobuf -I/opt/local/include -L/opt/local/lib/amd64 -L/opt/local/lib

# Instructions
all: server

server: packet.pb.cc
		$(CC) server.cpp packet.pb.cc $(LDFLAGS) -O2 -o server_release 

packet.pb.cc:
		protoc -I=. --cpp_out=. ./packet.proto

clean:
		rm packet.pb.cc
		rm packet.pb.h
		rm server_release
	
