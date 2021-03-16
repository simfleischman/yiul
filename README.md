# yiul

Haskell modularity tool. Named after the Hebrew word [יִעוּל](https://www.morfix.co.il/en/%D7%99%D7%A2%D7%95%D7%9C) meaning "making efficient, improving".

Build with `-fwrite-ide-info` to generate .hie files.
```
stack test --no-run-tests --ghc-options -fwrite-ide-info
```

Generate list of .hie files in current directory.
```
find . -name '*.hie' > hie-files.txt
```

Run the program.
```
stack run yiul -- --project-dir path/to/project --hie-files path/to/hie-files.txt --ghc-pkg path/to/ghc-pkg.txt
```
