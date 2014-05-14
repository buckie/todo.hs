build:
	make clean
	ghc todo-txt.hs
	mv todo-txt t
	make fake_todo

clean:
	rm -f *.o *.hi
	rm -f Todo/*.o Todo/*.hi
	rm -f t t.txt

fake_todo:
	cp t.txt.sample t.txt

seed:
	make fake_todo

# I'm so sorry about this:
fetch_deps:
	cabal install ansi-terminal
