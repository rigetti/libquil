.PHONY: all clean

OS := $(shell uname -s)

SBCL ?= sbcl

ifeq ($(OS), Darwin)
	LIBQUIL_TARGET = libquil.dylib
	CCFLAGS = -dynamiclib
else
	LIBQUIL_TARGET = libquil.so
	CCFLAGS = -shared
endif

# The library needs to embed the SBCL runtime. `make.sh` does not build a
# linkable runtime, so it has to be produced separately by running
# `make-shared-library.sh` in the SBCL source tree, and neither `install.sh` nor
# most package managers install the result. Look in the usual places for it, and
# let the user point at it directly with e.g. `make LIBSBCL=/path/to/libsbcl.a`.
#
# Which file to expect depends on the platform: on arm64 macOS the SBCL runtime
# is only built as a static archive (see SBCL's Config.arm64-darwin), so a
# shared libsbcl does not exist there at all.
SBCL_CORE_DIR := $(dir $(shell $(SBCL) --noinform --no-sysinit --no-userinit --non-interactive \
                                 --eval '(princ (namestring sb-ext:*core-pathname*))' 2>/dev/null))
LIBSBCL_SEARCH_DIRS := $(SBCL_HOME) $(SBCL_CORE_DIR) /usr/local/lib /usr/lib /opt/homebrew/lib
LIBSBCL_CANDIDATES := \
	$(foreach dir,$(LIBSBCL_SEARCH_DIRS),\
		$(dir)/libsbcl.a $(dir)/libsbcl.dylib $(dir)/libsbcl.so)

LIBSBCL ?= $(firstword $(wildcard $(LIBSBCL_CANDIDATES)))

# A static runtime must be linked whole: the entry points the generated
# libquil.c calls are reached only through the core, so without this the linker
# drops most of the archive. Its own dependencies have to be named explicitly
# too, since an archive records none. A shared runtime carries both properties
# already and just needs to be linked normally.
SBCL_STATIC_DEPS ?= $(shell pkg-config --libs libzstd 2>/dev/null || echo -lzstd) -lm -ldl -lpthread

ifeq ($(suffix $(LIBSBCL)), .a)
ifeq ($(OS), Darwin)
	LIBSBCL_LDFLAGS = -Wl,-force_load,$(LIBSBCL) $(SBCL_STATIC_DEPS)
else
	LIBSBCL_LDFLAGS = -Wl,--whole-archive $(LIBSBCL) -Wl,--no-whole-archive $(SBCL_STATIC_DEPS)
endif
else ifeq ($(OS), Darwin)
# Link by path rather than -lsbcl: make-shared-library.sh names its output
# libsbcl.so even on macOS, and the -l flag only ever looks for libsbcl.dylib or
# libsbcl.a.
	LIBSBCL_LDFLAGS = $(LIBSBCL)
else
	LIBSBCL_LDFLAGS = -L$(dir $(LIBSBCL)) -lsbcl
endif

all: $(LIBQUIL_TARGET)

libquil.core libquil.c libquil.h libquil.py: src/libquil.lisp src/qvm/*.lisp src/quilc/*.lisp
	$(SBCL) --dynamic-space-size 8192 --load "src/build-image.lisp"

$(LIBQUIL_TARGET): libquil.core libquil.c
ifeq ($(LIBSBCL),)
	@echo "error: no linkable SBCL runtime found."                                   >&2
	@echo "Searched for libsbcl.a, libsbcl.dylib and libsbcl.so in:"                 >&2
	@$(foreach dir,$(LIBSBCL_SEARCH_DIRS),echo "    $(dir)" >&2;)
	@echo ""                                                                         >&2
	@echo "Build one from an SBCL source tree of the SAME version as $(SBCL):"       >&2
	@echo "    sh make.sh --with-sb-linkable-runtime && sh make-shared-library.sh"   >&2
	@echo "then point make at the result, e.g.:"                                     >&2
	@echo "    make LIBSBCL=/path/to/sbcl/src/runtime/libsbcl.a"                     >&2
	@exit 1
endif
	$(CC) $(CCFLAGS) -o $@ libquil.c $(LIBSBCL_LDFLAGS)

clean:
	rm -f libquil.so libquil.c libquil.h libquil.core libquil.py libquil.dylib example
