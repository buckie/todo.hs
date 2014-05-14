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
	rm -f archive.txt t.txt

__post_build:
	mv todoTxt t
	make __fake_todo
	make __clean_junk

__clean_junk:
	rm -f *.o *.hi
	rm -f Todo/*.o Todo/*.hi

__fake_todo:
	cp t.txt.sample t.txt

seed:
	make __fake_todo

# I'm so sorry about this:
fetch_deps:
	cabal install ansi-terminal
