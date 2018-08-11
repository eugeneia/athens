ASD = $(shell find src/ -regex '[^\#]*\.asd' -printf '%p ')
VENDOR_ASD = $(shell find lib/ -regex '[^\#]*\.asd' -printf '%p ')
SOURCE_OBJECTS = $(shell find src/ -regex '[^\#]*\.lisp' -printf '%p ') $(ASD)

bin/athens: quicklisp bin build/athens.lisp $(SOURCE_OBJECTS)
	ccl -Q -b -n -l quicklisp/setup.lisp -l build/athens.lisp
	du -h bin/athens
bin:
	mkdir bin
quicklisp:
	ccl -Q -b -n -l lib/quicklisp/quicklisp.lisp \
		-e '(quicklisp-quickstart:install :path "quicklisp/")' \
		-e '(quit)'
	for asd in $(ASD) $(VENDOR_ASD); \
		do ln -s -v ../../$$asd quicklisp/local-projects/; done
clean:
	rm -rf bin
tidy: clean
	rm -rf quicklisp
.PHONY: clean tidy
