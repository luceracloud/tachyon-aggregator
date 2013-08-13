CC=g++
LD=-lzmq
LDFLAGS=-lzmq -lprotobuf -lfastbit
LDFLAGS_LCL=$(LDFLAGS) -I/opt/local/include -L/opt/local/lib/amd64 -L/opt/local/lib

.PHONY: rel

all: pckt listener

listener:
	$(CC) listener.cc pckt.pb.cc $(LDFLAGS_LCL) -g -o listener

pckt:
	protoc -I=. --cpp_out=. ./pckt.proto

csv_util:
	$(CC) make_csv.cc $(LDFLAGS_LCL) -o make_csv
	$(CC) combine_csv.cc $(LDFLAGS_LCL) -o combine_csv

clean:
	-rm listener
	-rm pckt.pb.h
	-rm pckt.pb.cc

rel:
	cp ./listener ./bin/

