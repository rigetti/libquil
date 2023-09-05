.PHONY: all clean

OS:=$(shell uname -s)
ifeq ($(OS), Darwin)
	LIBQUIL_TARGET = libquil.dylib
	CCFLAGS = -dynamiclib
else
	LIBQUIL_TARGET = libquil.so
	CCFLAGS = -shared
endif

all: $(LIBQUIL_TARGET)

libquil.core libquil.c libquil.h libquil.py: src/libquil.lisp src/qvm.lisp src/quilc.lisp
	sbcl --dynamic-space-size 4096 --load "src/build-image.lisp"

$(LIBQUIL_TARGET): libquil.core libquil.c
	gcc $(CCFLAGS) -o $@ libquil.c -lsbcl

clean:
	rm -f libquil.so libquil.c libquil.h libquil.core libquil.py libquil.dylib example
