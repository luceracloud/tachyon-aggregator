REBAR = $(shell pwd)/rebar3
PROJECT=tachyon

.PHONY: rel package version all

all: cp-hooks compile

cp-hooks:
	cp hooks/* .git/hooks

version:
	@echo "$(shell git symbolic-ref HEAD 2> /dev/null | cut -b 12-)-$(shell git log --pretty=format:'%h, %ad' -1)" > $(PROJECT).version

version_header: version
	@echo "-define(VERSION, <<\"$(shell cat $(PROJECT).version)\">>)." > apps/$(PROJECT)/include/$(PROJECT)_version.hrl

compile: version_header
	$(REBAR) compile

clean:
	$(REBAR) clean
	make -C rel/pkg clean

long-test:
	[ -d apps/$(PROJECT)/.eunit ] && rm -r apps/$(PROJECT)/.eunit || true
	$(REBAR) skip_deps=true -DEQC_LONG_TESTS eunit -v -r

qc:
	$(REBAR) -C rebar_eqc.config compile skip_deps=true eunit --verbose

eqc-ci: clean all
	$(REBAR) -D EQC_CI -C rebar_eqc_ci.config compile eunit skip_deps=true --verbose

eunit:
	$(REBAR) eunit

test: eunit
	$(REBAR) xref

quick-test:
	$(REBAR) eunit

rel:
	$(REBAR) as prod release

update:
	$(REBAR) update

package: update rel
	make -C rel/pkg package

###
### Docs
###
docs:
	$(REBAR) skip_deps=true doc

##
## Developer targets
##

xref: all
	$(REBAR) xref

stage : rel
	$(foreach dep,$(wildcard deps/* wildcard apps/*), rm -rf rel/$(PROJECT)/lib/$(shell basename $(dep))-* && ln -sf $(abspath $(dep)) rel/$(PROJECT)/lib;)


stagedevrel: dev1 dev2 dev3 dev4
	mkdir -p dev/dev{1,2,3}/data/{ipranges,datasets,packages,ring}
	$(foreach dev,$^,\
	  $(foreach dep,$(wildcard deps/* wildcard apps/*), rm -rf dev/$(dev)/lib/$(shell basename $(dep))-* && ln -sf $(abspath $(dep)) dev/$(dev)/lib;))

devrel: dev1 dev2 dev3 dev4


devclean:
	rm -rf dev

dev1 dev2 dev3 dev4: all
	mkdir -p dev
	(cd rel && $(REBAR) generate target_dir=../dev/$@ overlay_vars=vars/$@.config)


##
## Dialyzer
##
APPS = kernel stdlib sasl erts ssl tools os_mon runtime_tools crypto inets \
       xmerl webtool snmp public_key mnesia eunit syntax_tools compiler edoc

COMBO_PLT = $(HOME)/.$(PROJECT)_combo_dialyzer_plt

# DIALYZER_IGNORE="^\(riak_core\|leexinc.hrl\|pokemon_pb.erl\|meck_cover.erl\|meck.erl\|supervisor_pre_r14b04.erl\|webmachine_resource.erl\|uuid.erl\|gen_server2.erl\|folsom_vm_metrics.erl\|protobuffs_compile.erl\)"

check_plt: deps compile
	dialyzer --check_plt --plt $(COMBO_PLT) --apps $(APPS) \
		deps/*/ebin apps/*/ebin

build_plt: deps compile
	dialyzer --build_plt --output_plt $(COMBO_PLT) --apps $(APPS) \
		deps/*/ebin apps/*/ebin

dialyzer: deps compile
	@echo
	@echo Use "'make check_plt'" to check PLT prior to using this target.
	@echo Use "'make build_plt'" to build PLT prior to using this target.
	@echo
	@sleep 1
	dialyzer -Wno_return --plt $(COMBO_PLT) deps/*/ebin apps/*/ebin | grep -v -f dialyzer.mittigate

dialyzer-gui: deps compile
	dialyzer --gui -Wno_return --plt $(COMBO_PLT) deps/*/ebin apps/*/ebin
typer:
	typer --plt $(COMBO_PLT) deps/*/ebin apps/*/ebin

cleanplt:
	@echo
	@echo "Are you sure?  It takes about 1/2 hour to re-build."
	@echo Deleting $(COMBO_PLT) in 5 seconds.
	@echo
	sleep 5
	rm $(COMBO_PLT)

tags:
	find . -name "*.[he]rl" -print | etags -
