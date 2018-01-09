# dwarf-compiler
compiler of imperative language to dwarf-vm bytecode

# Build

**Tested on Debian**

You need `cabal-install`, `git`, `ghc` packages to build dwarf-compiler

Install the following packages if you don't have them:
```
cabal install parsec
cabal install data-binary-ieee754
```
For Arch instead of install `data-binary-ieee754` run:
```
yaourt -S haskell-data-binary-ieee754
```

Clone project
```
git clone https://github.com/merfemor/dwarf-compiler.git
cd dwarf-compiler
```
In the project directory, enter following commands to configure and build the project:
```
cabal build
```
If you have errors on Arch-based distros, you can try add `-dynamic` flag in `ghc-options` in `.cabal` file

If build succeeds, dwarf-compiler will be in the `./dist/build/dwsc` directory. You can run it:
```
./dist/build/dwsc/dwsc <source_file.dws>
```
or just
```
cabal run <source_file.dws>
```

Also you can install dwarf-compiler into your `.cabal` directory:
```
cabal install
```
Now you can run dwarf-compiler from anywhere (assuming that you have `.cabal/bin` in your PATH):
```
dwsc <source_file.dws>
```
