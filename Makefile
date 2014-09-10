build:
	cabal clean
	cabal configure
	cabal build
	make __fake_todo

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
	cabal install

seed:
	make __fake_todo

run:
	./dist/build/t/t

ls_todo:
	ag --ignore Makefile "TODO:"

ls_fix:
	ag --ignore Makefile "FIXME:"

__fake_todo:
	cp todo.txt.sample todo.txt
