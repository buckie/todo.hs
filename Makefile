build:
	cabal clean
	cabal build
	make __fake_todo

configure:
	cabal configure

clean:
	cabal clean
	rm -f archive.txt todo.txt

init:
	rm -rf .cabal-sandbox
	rm -rf cabal-sandbox-config
	cabal sandbox init
	make deps

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
