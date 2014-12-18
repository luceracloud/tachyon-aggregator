.PHONY: all clean console deps package

all: version
	./rebar compile

version:
	@echo "$(shell git symbolic-ref HEAD 2> /dev/null | cut -b 12-)-$(shell git log --pretty=format:'%h, %ad' -1)" > tachyon.version

version_header: version
	@echo "$(shell git symbolic-ref HEAD 2> /dev/null | cut -b 12-)-$(shell git log --pretty=format:'%h, %ad' -1)" > tachyon.version

clean:
	./rebar clean

test: xref
	./rebar eunit skip_deps=true -r

xref: all
	./rebar xref skip_deps=true -r

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

package: rel
	make -C rel/pkg
