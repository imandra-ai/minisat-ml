
FLAGS= --profile=release
J?=3

build:
	@dune build @all $(FLAGS) -j $(J)

test:
	@dune runtest $(FLAGS) --force --no-buffer

clean:
	@dune clean

dev: build test

watch:
	@dune build @all -w $(FLAGS)

fmt:
	@ocamlformat -i $(shell find src -name '*.ml' -or -name '*.mli')

DATE=$(shell date +%FT%H:%M)
TEST_OPTS?= -j $(J) --junit test.xml --progress

logitest-quick:
	@mkdir -p snapshots
	@logitest run --meta=`git rev-parse HEAD` \
	  --summary snapshots/bench-$(DATE).txt \
	  --csv snapshots/bench-$(DATE).csv \
	  -c benchs/conf.toml $(TEST_OPTS)

logitest-basic:
	@mkdir -p snapshots
	@logitest run --meta=`git rev-parse HEAD` \
	  --summary snapshots/bench-basic-$(DATE).txt \
	  --csv snapshots/bench-basic-$(DATE).csv \
	  -c benchs/conf.toml $(TEST_OPTS) --profile=bench-basic


