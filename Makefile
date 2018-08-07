.PHONY: all build unit_test integration_test clean

build:
	dune build

all: build

unit_test:
	dune exec test/test.exe

integration_test:
	dune exec integration_test/test.exe

clean:
	rm -rf _build *.install
