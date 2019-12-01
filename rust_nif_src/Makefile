# Based on c_src.mk from erlang.mk by Loic Hoguin <essen@ninenines.eu>

CURDIR := $(shell pwd)
BASEDIR := $(abspath $(CURDIR)/..)

RUST_SRC_OUTPUT ?= $(CURDIR)/../apps/aoc/priv/libaoc.so
SOURCES := $(shell find $(C_SRC_DIR) -type f \( -name "*.rs" \))

.DEFAULT_GOAL := $(RUST_SRC_OUTPUT)

$(RUST_SRC_OUTPUT): $(SOURCES)
	@cargo build --release
	@cp target/release/libaoc.so $(RUST_SRC_OUTPUT)

clean:
	@cargo clean
	@rm -rf $(RUST_SRC_OUTPUT)
