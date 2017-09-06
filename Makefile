.PHONY: default clean test tree

default:
	jbuilder build @install

clean:
	jbuilder clean

test:
	jbuilder runtest

tree:
	tree -aI '_build|.git'
