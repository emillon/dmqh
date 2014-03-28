.PHONY: all clean

all:
	ocp-build -init

clean:
	ocp-build -clean
