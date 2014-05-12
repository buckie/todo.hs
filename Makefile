clean:
	rm -f todo.o todo.hi todo

fake_todo:
	cp t.txt.sample t.txt

seed:
	make fake_todo
