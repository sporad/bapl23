# Lua version and release.
V= 5.4
R= $V.6

all: linux

linux:
	@cd src && $(MAKE) $@ && cd .. && cp ./src/lua ../..

test clean:
	@cd src && $(MAKE) $@

.PHONY: all linux test clean
