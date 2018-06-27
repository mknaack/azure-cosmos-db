.PHONY: all build unit_test integration_test clean

build:
	jbuilder build --dev

all: build

unit_test:
	jbuilder exec test/test.exe

integration_test:
	jbuilder exec integration_test/test.exe

clean:
	rm -rf _build *.install
