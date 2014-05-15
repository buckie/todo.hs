# todo.hs

An alternative CLI app for the popular (well, among geeks anyway) [Todo.txt](http://todotxt.com/) ecosystem.

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
export TODO_TXT_PATH=”/path/to/todo.txt”
export TODO_ARCHIVE_PATH=”/path/to/archive.txt”
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

