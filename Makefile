build:
	ghc todo-txt.hs

clean:
	rm -f *.o *.hi
	rm -f todo-txt t.txt

fake_todo:
	cp t.txt.sample t.txt

seed:
	make fake_todo
