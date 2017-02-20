bin/athens: bin build/athens.lisp $(SOURCE_OBJECTS)
	ccl -Q -b -n -l build/athens.lisp
bin:
	mkdir bin
SOURCE_OBJECTS = $(shell find . -regex '[^\#]*\.lisp' -printf '%P ' \
			&& find . -regex '[^\#]*\.asd' -printf '%P ')
clean:
	rm -rf bin
.PHONY: clean
