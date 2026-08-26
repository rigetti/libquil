TEST_SRCS = $(wildcard *.c)
TEST_OBJS = $(TEST_SRCS:.c=)
BUILD_DIR = ../..
RUNTIME_DIR = $(BUILD_DIR)/runtime
# libquil is the generated bindings; libsbcl_librarian is the runtime that brings up
# the Lisp image (and supplies get_error_message).
CCFLAGS = -lquil -lsbcl_librarian -L$(BUILD_DIR) -L$(RUNTIME_DIR) -I$(BUILD_DIR) -I$(RUNTIME_DIR)

.PHONY: all clean

all: $(TEST_SRCS:.c=)

%: %.c
	gcc -Wall -Wextra $< -o $@ $(CCFLAGS)
	# patchelf --set-rpath '$$ORIGIN/../../' $@

test-%: $(TEST_OBJS)
	@echo "Testing" $(patsubst test-%,%,$@)
	./$(patsubst test-%,%,$@)
	@echo

test: $(patsubst %,test-%,$(TEST_OBJS))

clean:
	rm -f $(TEST_SRCS:.c=)
