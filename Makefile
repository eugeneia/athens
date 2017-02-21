bin/athens: quicklisp bin build/athens.lisp $(SOURCE_OBJECTS)
	ccl -Q -b -n -l quicklisp/setup.lisp -l build/athens.lisp
	du -h bin/athens
bin:
	mkdir bin
SOURCE_OBJECTS = $(shell find src/ -regex '[^\#]*\.lisp' -printf '%P ' \
			&& find src/ -regex '[^\#]*\.asd' -printf '%P ')
quicklisp:
	ccl -Q -b -n -l lib/quicklisp/quicklisp.lisp \
		-e '(quicklisp-quickstart:install :path "quicklisp/")' \
		-e '(quit)'
	ln -s -v ../../src quicklisp/local-projects/athens
	ln -s -v ../../lib/configuration quicklisp/local-projects/configuration
	ln -s -v ../../lib/trivial-feed quicklisp/local-projects/trivial-feed
	ln -s -v ../../lib/httpd0 quicklisp/local-projects/httpd0
	ln -s -v ../../lib/erlangen quicklisp/local-projects/erlangen
clean:
	rm -rf bin
tidy: clean
	rm -rf quicklisp
.PHONY: clean tidy
