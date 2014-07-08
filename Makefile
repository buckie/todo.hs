build:
	cabal clean
	cabal install -j
	cabal build
	make __fake_todo

clean:
	cabal clean
	rm -f archive.txt todo.txt

install:
	mv ./dist/build/todo-hs/todo-hs /usr/local/bin/t

ls_todo:
	ag --ignore Makefile "TODO:"

ls_fix:
	ag --ignore Makefile "FIXME:"

seed:
	make __fake_todo

__fake_todo:
	cp todo.txt.sample todo.txt
