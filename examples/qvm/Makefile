TEST_SRCS = $(wildcard *.c)
TEST_OBJS = $(TEST_SRCS:.c=)
BUILD_DIR = ../..
CCFLAGS = -lsbcl -lquil -L$(BUILD_DIR) -I$(BUILD_DIR)

OS:=$(shell uname -s)
ifeq ($(OS), Darwin)
	CCFLAGS += -pagezero_size 0x100000
endif

.PHONY: all clean

all: $(TEST_SRCS:.c=)

%: %.c
	gcc $< -o $@ $(CCFLAGS)
	# patchelf --set-rpath '$$ORIGIN/../../' $@

test-%: $(TEST_OBJS)
	@echo "Testing" $(patsubst test-%,%,$@)
	./$(patsubst test-%,%,$@)
	@echo

test: $(patsubst %,test-%,$(TEST_OBJS))

clean:
	rm -f $(TEST_SRCS:.c=)
