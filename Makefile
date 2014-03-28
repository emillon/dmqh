.PHONY: all clean check

all:
	ocp-build -init

clean:
	ocp-build -clean

check: all
	./_obuild/dmqh_tests/dmqh_tests.asm

js: all
	js_of_ocaml ./_obuild/dmqh_web/dmqh_web.byte
