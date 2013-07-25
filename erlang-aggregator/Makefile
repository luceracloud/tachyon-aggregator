.PHONY: all clean console

all:
	CC=gcc LDFLAGS="-lzmq -L/opt/local/lib" CFLAGS="-lzmq -I/opt/local/include" ./rebar compile
#	LD_LIBRARY_PATH=/opt/local/lib CC=gcc LDFLAGS="-lzmq -I/opt/local/include -L/opt/local/lib/amd64 -L/opt/local/lib" CFLAGS="-lzmq -I/opt/local/include -L/opt/local/lib/amd64 -L/opt/local/lib" ./rebar compile
#
clean:
	./rebar clean

console:
	LD_LIBRARY_PATH=/opt/local/lib:./deps/erlzmq/deps/zeromq3/src/.libs/ erl -pa deps/*/ebin -pa ebin
