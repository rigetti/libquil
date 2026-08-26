.PHONY: all clean runtime install

OS := $(shell uname -s)

SBCL ?= sbcl
CC ?= cc

ifeq ($(OS), Darwin)
	SHARED_SUFFIX = .dylib
	SHARED_FLAGS = -dynamiclib
else
	SHARED_SUFFIX = .so
	SHARED_FLAGS = -shared
endif

LIBQUIL_TARGET = libquil$(SHARED_SUFFIX)

# libquil uses sbcl-librarian, which splits the work in two:
#
#   runtime/libsbcl_librarian$(SHARED_SUFFIX)  the SBCL runtime; a constructor
#                                              initializes Lisp when it is loaded
#   runtime/libquil.core                       the Lisp image, holding libquil and
#                                              its dependencies
#   libquil$(SHARED_SUFFIX)                    the generated C bindings
#
# Both halves are built here, so `make` alone still produces a usable artifact.
RUNTIME_DIR := runtime
RUNTIME_LIB := $(RUNTIME_DIR)/libsbcl_librarian$(SHARED_SUFFIX)
CORE := $(RUNTIME_DIR)/libquil.core

# Passed to scripts/install.sh by `make install`. Override for a prefix you can
# write without sudo, e.g. make install PREFIX="$HOME/.local"
PREFIX ?= /usr/local

SBCL_LIBRARIAN_DIR := $(shell $(SBCL) --noinform --non-interactive \
    --eval '(require :asdf)' \
    --eval '(princ (namestring (asdf:system-source-directory "sbcl-librarian")))' 2>/dev/null)

# The linkable SBCL runtime. `make.sh` does not build one by default, and neither
# Homebrew's nor Ubuntu's sbcl package ships it, so it comes from a source tree
# built with
# `make-shared-library.sh`; `install.sh` puts it in SBCL's home directory. Note
# that SBCL names it libsbcl.so on every platform, including macOS.
SBCL_CORE_DIR := $(dir $(shell $(SBCL) --noinform --no-sysinit --no-userinit --non-interactive \
                                 --eval '(princ (namestring sb-ext:*core-pathname*))' 2>/dev/null))
LIBSBCL_SEARCH_DIRS := $(SBCL_HOME) $(SBCL_CORE_DIR) $(SBCL_CORE_DIR).. \
                       /usr/local/lib /usr/lib /opt/homebrew/lib
LIBSBCL ?= $(firstword $(wildcard \
    $(foreach dir,$(LIBSBCL_SEARCH_DIRS),$(dir)/libsbcl.so $(dir)/libsbcl.dylib)))

# libsbcl needs zstd for core compression; pkg-config knows where it is on systems
# that install it outside the default search path (Homebrew, in particular).
ZSTD_LIBS ?= $(shell pkg-config --libs libzstd 2>/dev/null || echo -lzstd)

all: $(LIBQUIL_TARGET)

runtime: $(RUNTIME_LIB)

# One image produces everything Lisp-side: libquil's bindings, the runtime's
# bindings, and the core that backs both.
$(CORE) libquil.c libquil.h $(RUNTIME_DIR)/sbcl_librarian.c: src/libquil.lisp src/qvm/*.lisp src/quilc/*.lisp src/build-image.lisp
	mkdir -p $(RUNTIME_DIR)
	$(SBCL) --dynamic-space-size 8192 --non-interactive --load "src/build-image.lisp"
	# The core is named after the aggregate library that defines its exports
	# (libquil-core); publish it beside the runtime under the name the runtime
	# was compiled to look for.
	mv libquil_core.core $(CORE)

# SBCL_LIBRARIAN_CORE_NAME is the core name the runtime is compiled to look for
# next to itself. It defaults to sbcl_librarian.core; libquil never builds a core
# by that name, so the runtime is pointed at libquil.core instead.
$(RUNTIME_LIB): $(RUNTIME_DIR)/sbcl_librarian.c
ifeq ($(LIBSBCL),)
	@echo "error: no linkable SBCL runtime (libsbcl.so) found."                        >&2
	@echo "Searched:"                                                                  >&2
	@$(foreach dir,$(LIBSBCL_SEARCH_DIRS),echo "    $(dir)" >&2;)
	@echo "Build one from the SBCL source tree that produced $(SBCL) -- a matching"     >&2
	@echo "version is not enough, the build IDs have to agree:"                        >&2
	@echo "    sh make.sh --with-sb-linkable-runtime && sh make-shared-library.sh"     >&2
	@echo "then re-run make, or pass LIBSBCL=/path/to/libsbcl.so"                      >&2
	@exit 1
endif
	mkdir -p $(RUNTIME_DIR)
	cp $(LIBSBCL) $(RUNTIME_DIR)/libsbcl.so
	cp "$(SBCL_LIBRARIAN_DIR)lib/sbcl_librarian_err.h" $(RUNTIME_DIR)/
	cd $(RUNTIME_DIR) && $(CC) $(SHARED_FLAGS) -fPIC -o libsbcl_librarian$(SHARED_SUFFIX) \
	    sbcl_librarian.c \
	    "$(SBCL_LIBRARIAN_DIR)lib/entry_point.c" \
	    -DLIBSBCL_LIBRARIAN_API_BUILD \
	    -DSBCL_LIBRARIAN_CORE_NAME='"libquil.core"' \
	    -I. -I"$(SBCL_LIBRARIAN_DIR)lib" -L. -lsbcl $(ZSTD_LIBS)

$(LIBQUIL_TARGET): libquil.c $(CORE) $(RUNTIME_LIB)
	$(CC) $(SHARED_FLAGS) -fPIC -o $@ libquil.c \
	    -I. -I$(RUNTIME_DIR) -I"$(SBCL_LIBRARIAN_DIR)lib" \
	    -L$(RUNTIME_DIR) -lsbcl_librarian

# Delegates to the release installer so the layout, the prefix handling and the
# post-install hint have one implementation. --from makes it install this build
# tree rather than downloading.
install: all
	scripts/install.sh --from . --prefix "$(PREFIX)"

clean:
	rm -rf $(RUNTIME_DIR) build
	rm -f libquil.so libquil.dylib libquil.h libquil.c libquil.core libquil.py example
