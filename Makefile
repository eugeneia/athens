bin/athens: bin src/build/athens.lisp $(SOURCE_OBJECTS)
	ccl -Q -b -n -l src/build/athens.lisp
bin:
	mkdir bin
SOURCE_OBJECTS = $(shell find src/ -regex '[^\#]*\.lisp' -printf '%P ' \
			&& find src/ -regex '[^\#]*\.asd' -printf '%P ')
clean:
	rm -rf bin
.PHONY: clean
