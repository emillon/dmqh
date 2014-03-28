.PHONY: all clean check

all:
	ocp-build -init

clean:
	ocp-build -clean

check: all
	./_obuild/dmqh_tests/dmqh_tests.asm
