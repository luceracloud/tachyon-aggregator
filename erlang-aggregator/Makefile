.PHONY: all clean console

all:
	CC=gcc LDFLAGS="-lzmq -L/opt/local/lib" CFLAGS="-lzmq -I/opt/local/include" ./rebar compile
#	LD_LIBRARY_PATH=/opt/local/lib CC=gcc LDFLAGS="-lzmq -I/opt/local/include -L/opt/local/lib/amd64 -L/opt/local/lib" CFLAGS="-lzmq -I/opt/local/include -L/opt/local/lib/amd64 -L/opt/local/lib" ./rebar compile
#

osx:
	CC=gcc LDFLAGS="-lzmq -L/usr/local/lib" CFLAGS="-lzmq -I/usr/local/include" ./rebar compile
clean:
	./rebar clean

console:
	LD_LIBRARY_PATH=/opt/local/lib erl -pa deps/*/ebin -pa ebin

run:
	LD_LIBRARY_PATH=/opt/local/lib erl -pa deps/*/ebin -pa ebin -config london -s erlaggregator

ny4: all
	LD_LIBRARY_PATH=/opt/local/lib erl -pa deps/*/ebin -pa ebin -config ny4 -s erlaggregator

local: all
	LD_LIBRARY_PATH=/opt/local/lib erl -pa deps/*/ebin -pa ebin -config local -s erlaggregator
