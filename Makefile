build:
	cabal clean
	cabal configure
	cabal build
	make __fake_todo

clean:
	cabal clean
	rm -f archive.txt todo.txt

init:
	cabal sandbox init

deps:
	cabal install --only-dependencies

install:
	mv ./dist/build/todo-hs/todo-hs /usr/local/bin/t

seed:
	make __fake_todo

ls_todo:
	ag --ignore Makefile "TODO:"

ls_fix:
	ag --ignore Makefile "FIXME:"

__fake_todo:
	cp todo.txt.sample todo.txt
