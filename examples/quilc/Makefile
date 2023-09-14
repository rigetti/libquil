TEST_SRCS = $(wildcard *.c)
BUILD_DIR = ../..
CCFLAGS = -lsbcl -lquil -L$(BUILD_DIR) -I$(BUILD_DIR)

OS:=$(shell uname -s)
ifeq ($(OS), Darwin)
	CCFLAGS += -pagezero_size 0x100000
endif

.PHONY: all clean

all: $(TEST_SRCS:.c=)

%: %.c ../../libquil.so
	gcc $< -o $@ $(CCFLAGS)
	patchelf --set-rpath '$$ORIGIN/../../' $@

clean:
	rm -f $(TEST_SRCS:.c=)
