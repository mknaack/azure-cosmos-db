.PHONY: integration_test
integration_test:
	jbuilder exec integration_test/test.exe

.PHONY: unit_test
unit_test:
	jbuilder exec test/test.exe
