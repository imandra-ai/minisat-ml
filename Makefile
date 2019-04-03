
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

DATE=$(shell date +%FT%H:%M)
TEST_OPTS?= -j $(J) --junit test.xml

logitest-quick:
	@mkdir -p snapshots
	@logitest run --meta=`git rev-parse HEAD` \
	  --summary snapshots/bench-$(DATE).txt \
	  --csv snapshots/bench-$(DATE).csv \
	  -c benchs/conf.toml $(TEST_OPTS)

