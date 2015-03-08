VERSION=0.1.0
COMPONENT_INTERNAL=tachyon
COMPONENT=tachyon
DEPS="erlang>=16.1.2"

include ../../deps/fifo_utils/priv/pkg.mk

.PHONY: package prepare clean

prepare:
	-rm -r $(STAGE_DIR)/$(COMPONENT)
	cp -r ../$(COMPONENT_INTERNAL) $(STAGE_DIR)/$(COMPONENT)

package: prepare $(FILE).tgz

clean: clean-pkg
	-rm *.tgz
