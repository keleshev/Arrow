.PHONY: build clean test tree

default: test

build:
	jbuilder build @install

clean:
	jbuilder clean

test:
	jbuilder runtest

tree:
	tree -aI '_build|.git'

test_ppx:
	#jbuilder build && ocamlc -dsource -dparsetree -ppx ~/Developer/arrow/_build/install/default/bin/arrow-ppx ppx/test.ml
	jbuilder build && ocamlc -dsource -dparsetree -ppx ./_build/default/ppx/main.exe ppx/test.ml
