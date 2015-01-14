# todo.hs

An alternative CLI app for the popular (well, among geeks anyway)
[Todo.txt](http://todotxt.com/) ecosystem.

Some improvements are:

* doesn’t mess up todo.txt, keeps todos where they are
* maintains todo ids across runs
* ludicrous speed
* most commands take multiple `todiID`s (e.g., `t pri A 1 2 12 32 42` will
  prioritise all the todos given)
* uses the `todo.txt` in the current directory if there is one (useful for
  project-specific todos)

Mainly to teach myself haskell.


# Setup

Todo.hs expects the two environment variables to be set. Put the following in
your `.bashrc` or `.zshrc`:

```
export TODO_TXT_PATH="/path/to/todo.txt"
export DONE_TXT_PATH="/path/to/archive.txt"
```

# Building

```bash
$ git checkout https://github.com/vise890/todo.hs

$ # optional (if you just wanna test it out):
$ cabal sandbox init

$ cabal install
```

# Just Running it
```bash
$ cabal repl

$ cabal run
```

# Usage
```
$ t add "(A) become ultra-zen"
$ t a "(B) go to easter island"

$ t prioritise A 1 2 3
$ t pri C 2 4

$ t complete 1 2 3 5
$ t do 11 1 10

$ t archive
$ t ar

...
# for more, read the dispatch function in Main.hs
```

