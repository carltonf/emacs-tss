EMACS ?= emacs
CASK ?= cask

SOURCES = tss.el

all:
	${MAKE} clean
	${MAKE} test
	${MAKE} compile
	${MAKE} test
	${MAKE} clean

compile: ${SOURCES:.el=.elc}
%.elc: %.el
	${CASK} exec ${EMACS} -Q --batch -L . --eval "(batch-byte-compile)" $<

# pass TESTS as make cmd/env variable to configure which tests you want to run.
# every test is a file composed of several exepectations under test/
TESTS ?= $(shell find test -type f -name "*.el")
test: compile
	ret=0 ; \
	outfile=/tmp/.elisp-test-result ; \
	for f in ${TESTS}; do \
	    test -f $$outfile && rm -f $$outfile ; \
		${CASK} exec ${EMACS} -Q --batch -L . -l $$f -f batch-expectations $$outfile || ret=1 ; \
	    test -f $$outfile && cat $$outfile ; \
	done ; \
	test $$ret -eq 0

clean:
	rm -f tss.elc

.PHONY: all compile test clean
