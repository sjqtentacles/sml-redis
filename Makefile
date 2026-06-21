# sml-redis build
#
#   make            build the test binary with MLton (default)
#   make test       build + run tests under MLton
#   make test-poly  run tests under Poly/ML (use-and-run; no link step)
#   make all-tests  run the suite under both compilers
#   make example    build + run examples/demo.sml
#   make clean      remove build artifacts
#
# Layout B (dependent): own sources live in src/; sml-buffer is vendored
# under lib/ and loaded first, in dependency order.

MLTON      ?= mlton
POLY       ?= poly
BIN        := bin
BUFDIR     := lib/github.com/sjqtentacles/sml-buffer
TEST_MLB   := test/sources.mlb
SRCS       := $(wildcard $(BUFDIR)/* src/* test/*.sml) $(TEST_MLB)

.PHONY: all test poly test-poly all-tests example clean

all: $(BIN)/test-mlton

example: $(BIN)/demo
	./$(BIN)/demo

$(BIN)/demo: $(SRCS) examples/demo.sml examples/sources.mlb | $(BIN)
	$(MLTON) -output $@ examples/sources.mlb

$(BIN)/test-mlton: $(SRCS) | $(BIN)
	$(MLTON) -output $@ $(TEST_MLB)

test: $(BIN)/test-mlton
	$(BIN)/test-mlton

# Poly/ML has no native .mlb support; the suite runs at top level and exits on
# its own. Load the vendored sml-buffer first, then the redis sources, then the
# test driver.
poly test-poly:
	printf 'use "$(BUFDIR)/buffer.sig";\nuse "$(BUFDIR)/buffer.sml";\nuse "src/redis.sig";\nuse "src/redis.sml";\nuse "test/harness.sml";\nuse "test/test.sml";\nuse "test/entry.sml";\nuse "test/main.sml";\n' | $(POLY) -q --error-exit

all-tests: test test-poly

$(BIN):
	mkdir -p $(BIN)

clean:
	rm -f $(BIN)/test-mlton $(BIN)/demo
