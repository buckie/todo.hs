# todo.hs

An alternative CLI app for the popular (well, among geeks anyway) [Todo.txt](http://todotxt.com/) ecosystem.

Some improvements are:

* doesn’t mess up todo.txt, keeps todos where they are
* maintains todo ids across runs
* ludicrous speed
* most commands take multiple `todiID`s (e.g., `t pri A 1 2 12 32 42` will prioritise all the todos given)
* uses the `todo.txt` in the current directory if there is one (useful for project-specific todos)

Mainly to teach myself haskell.

# Installation

```bash
$ git checkout ...
$ cd todo.hs
$ make init
$ make
$ make install
$ t
```

Todo.hs expects the two environment variables to be set. Put the following in your `.bashrc` or `.zshrc`:

```
export TODO_TXT_PATH="/path/to/todo.txt"
export DONE_TXT_PATH="/path/to/archive.txt"
```

# Usage
```
$ t add
$ t a

$ t prioritise
$ t pri

$ t complete
$ t do

$ t archive
$ t ar

...
# for more, read the dispatch function in Main.hs
```

