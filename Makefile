LUADIR=./luasrc/lua-5.4.6
LPEGDIR=./luasrc/lpeg-1.1.0

LUA=./lua
LPEG=lpeg.so

TESTS=$(wildcard Test/*.lua)

all:
	cd $(LUADIR) && make
	cd $(LPEGDIR) && make

test: $(TESTS)
	for i in $^; do echo $$i; $(LUA) ./$$i || exit 1; echo; done

clean:
	cd $(LUADIR) && make clean
	cd $(LPEGDIR) && make clean

.PHONY: all test clean
