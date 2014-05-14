build:
	make clean
	ghc todoTxt.hs
	mv todoTxt t
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
