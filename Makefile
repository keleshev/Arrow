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
