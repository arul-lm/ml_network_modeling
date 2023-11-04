all:
	@dune build @all

format:
	@dune build @fmt --auto-promote

WATCH ?= @all
watch:
	@dune build $(WATCH) -w

clean:
	@dune clean

run:
	@dune exec ml_network_modeling
