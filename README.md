# dwarf-compiler
compiler of imperative language to dwarf-vm bytecode

# Build
You need `cabal` package to build dwarf-compiler

Clone project
```
git clone https://github.com/merfemor/dwarf-compiler.git
cd dwarf-compiler
```
In the project directory, enter following commands to configure and build the project:
```
cabal configure
cabal build
```
If build succeeds, dwarf-compiler will be in the `./dist/build/dwarf-compiler` directory. You can run it:
```
./dist/build/dwarf-compiler/dwarf-compiler
```
Also you can install dwarf-compiler into your `.cabal` directory:
```
cabal install
```
Now you can run dwarf-compiler from anywhere (assuming that you have `.cabal/bin` in your PATH):
```
dwarf-compiler
```
