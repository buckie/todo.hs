# todo.hs

An alternative CLI app for the popular (well, among geeks anyway) [Todo.txt](http://todotxt.com/) ecosystem.

Some improvements are:

* doesn’t mess up todo.txt, keeps todos where they are
* maintains todo ids across runs
* Ludicrous speed

Mainly to teach myself haskell.

# Installation

```
git checkout ...
cd todo.hs
make
./t
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

...
```

