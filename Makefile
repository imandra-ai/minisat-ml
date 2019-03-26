
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
	@dune build @all -w
