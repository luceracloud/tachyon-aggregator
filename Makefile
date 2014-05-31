.PHONY: all clean console deps pkg

all:
	./rebar compile

clean:
	./rebar clean

test: xref
	./rebar eunit skip_deps=true

xref: all
	./rebar xref skip_deps=true

console:
	erl -pa deps/*/ebin -pa apps/*/ebin

run:
	erl -pa deps/*/ebin -pa apps/*/ebin -config london -s tachyon

ny4: all
	erl -pa deps/*/ebin -pa apps/tachyon/ebin -config ny4 -s tachyon

local: all
	erl -pa deps/*/ebin -pa apps/*/ebin -config local -s tachyon

rel: all
	-rm -r rel/tachyon/
	(cd rel; ../rebar generate)

deps:
	./rebar get-deps

pkg:
	make -c rel/pkg
