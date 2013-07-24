HMAKE := cabal
CWD := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))

all:
	$(HMAKE) configure
	$(HMAKE) build
	$(HMAKE) install --prefix=$(CWD)

clean:
	$(HMAKE) clean
	rm -rfv bin share
