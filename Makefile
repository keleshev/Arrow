default:
	jbuilder build @install

clean:
	jbuilder clean

tree:
	tree -aI '_build|.git'
