# yiul

Haskell modularity tool.

Build with `-fwrite-ide-info` to generate .hie files.
```
stack test --no-run-tests --ghc-options -fwrite-ide-info
```

Generate list of .hie files in current directory.
```
find $(pwd) -name '*.hie' > hie-files.txt
```
