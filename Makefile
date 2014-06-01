build:
	make clean
	ghc todoTxt.hs
	make __post_build

llvm_build:
	make clean
	ghc -fllvm todoTxt.hs
	make __post_build

clean:
	make __clean_junk
	rm -f t
	rm -f archive.txt todo.txt

install:
	mv ./t /usr/local/bin

ls_todo:
	ag --ignore Makefile "TODO:"

ls_fix:
	ag --ignore Makefile "FIXME:"

seed:
	make __fake_todo

__post_build:
	mv todoTxt t
	make __fake_todo
	make __clean_junk

__clean_junk:
	rm -f *.o *.hi
	rm -f TodoList/*.o TodoList/*.hi

__fake_todo:
	cp todo.txt.sample todo.txt

# I'm so sorry about this:
# FIXME: move the whole thing to a cabal build
fetch_deps:
	cabal update
	cabal install ansi-terminal
