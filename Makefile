LISP ?= sbcl

all: test

run:
	ros run --load ~/.sbclrc --load "run.lisp"

build:
	$(LISP)	--non-interactive \
		--load clevelib.asd \
		--eval '(ql:quickload :clevelib)' \
		--eval '(asdf:make :clevelib)'

test:
	ros run --load ~/.sbclrc \
		--non-interactive \
		--load run-tests.lisp
