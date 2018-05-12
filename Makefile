.PHONY: all vendor clean pong pong-sbcl pong-ccl

# All -------------------------------------------------------------------------
all: pong

# Vendor ----------------------------------------------------------------------
vendor/quickutils.lisp: vendor/make-quickutils.lisp
	cd vendor && sbcl --noinform --load make-quickutils.lisp  --eval '(quit)'

vendor: vendor/quickutils.lisp

# Clean -----------------------------------------------------------------------
clean:
	rm -rf bin build

# Build -----------------------------------------------------------------------
bin:
	mkdir -p bin

build:
	mkdir -p build

pong-sbcl: bin build
	echo ':sattyrday/001-pong' | /usr/local/bin/sbcl --noinform --load "src/build.lisp"
	rm -rf build/pong
	mv bin build/pong

pong-ccl: bin
	echo ':sattyrday/001-pong' | /usr/local/bin/ccl64 --load "src/build.lisp"
	rm -rf build/pong
	mv bin build/pong

pong: pong-sbcl
