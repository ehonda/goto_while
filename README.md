# Installation
The Data.Natural package is needed to run the programs, it can be found at <https://hackage.haskell.org/package/natural-numbers-0.1.2.0/docs/Data-Natural.html> or via cabal:

```
cabal install natural-numbers
```

# Goto Programs
Goto Programs are Lists of Goto Commands. Here is an example of what can be done with them:

```
Prelude> :l GotoPrograms.hs
[1 of 2] Compiling Store            ( Store.hs, interpreted )
[2 of 2] Compiling GotoPrograms     ( GotoPrograms.hs, interpreted )
Ok, 2 modules loaded.
*GotoPrograms> p = [Inc 0, Inc 0, Stop]
*GotoPrograms> run_with p []
2
*GotoPrograms> debug_run_with p [2, 3] 5
[2,2,3,0,0,0]
*GotoPrograms> pretty_print p math_symbols1
0: Inc n
1: Inc n
2: Stop
```

The functions ```run_with``` and ```debug_run_with``` run a Goto Program with the specified inputs and show the output/the content of the first n registers.
The function ```pretty_print``` prints a prettified version of the Program (opposed to the raw ```show```) and takes a ```RegisterSymbols``` Parameter which will be explained in the next section.
There are also implementations of some basic Goto Programs in GotoPrograms.hs:

```
*GotoPrograms> p = plus
*GotoPrograms> run_with p [5, 6]
11
*GotoPrograms> pretty_print p math_symbols2
0: GotoZ x 4
1: Dec x
2: Inc n
3: Goto 0
4: GotoZ y 8
5: Dec y
6: Inc n
7: Goto 4
8: Stop
```

# Register Symbols
The type ```RegisterSymbols``` is just a map from  ```Natural``` to ```String```. It can be used to print symbolic names instead of the register numbers in prettified output.

```
*GotoPrograms> p = [Inc 0, Inc 1, Inc 2, Stop]
*GotoPrograms> syms = set_syms default_symbols [(1, "a"), (2, "b")]
*GotoPrograms> pretty_print p syms
0: Inc 0
1: Inc a
2: Inc b
3: Stop
*GotoPrograms> pretty_print p math_symbols2
0: Inc n
1: Inc x
2: Inc y
3: Stop
```

The ```default_symbols``` print every register as its number. In the first example we change this to print "a" and "b" for registers 1 and 2. The second example uses a predefined ```RegisterSymbols```.

# While Programs
While Programs are formed as a tree of basic expressions (The programs' AST). Some example usages:

```
*GotoPrograms> :l WhilePrograms.hs
[1 of 2] Compiling Store            ( Store.hs, interpreted )
[2 of 2] Compiling WhilePrograms    ( WhilePrograms.hs, interpreted )
Ok, 2 modules loaded.
*WhilePrograms> p = While 1 $ Seq (Dec 1) (Inc 0)
*WhilePrograms> p
While 1 (Seq (Dec 1) (Inc 0))
*WhilePrograms> run_with p [5]
5
*WhilePrograms> debug_run_with p [5] 5
[5,0,0,0,0,0]
*WhilePrograms> pretty_print p math_symbols1
While x {
  Dec x;
  Inc n;
}
```

Some basic functions are implemented:

```
*WhilePrograms> q = times
*WhilePrograms> pretty_print q $ set_sym math_symbols2 (3, "tmp")
While x {
  Dec x;
  While y {
    Dec y;
    Inc n;
    Inc tmp;
  }
  While tmp {
    Dec tmp;
    Inc y;
  }
}
```

# Transpilation
The Transpile module implements transpilation functions from Goto Programs to While Programs and vice versa:

```
*WhilePrograms> :l Transpile.hs
[1 of 4] Compiling Store            ( Store.hs, interpreted )
[2 of 4] Compiling GotoPrograms     ( GotoPrograms.hs, interpreted )
[3 of 4] Compiling WhilePrograms    ( WhilePrograms.hs, interpreted )
[4 of 4] Compiling Transpile        ( Transpile.hs, interpreted )
Ok, 4 modules loaded.
*Transpile> p = transpile_w W.plus
*Transpile> G.pretty_print p math_symbols2
0: GotoZ x 4
1: Dec x
2: Inc n
3: Goto 0
4: GotoZ y 8
5: Dec y
6: Inc n
7: Goto 4
8: Stop
*Transpile> q = transpile_g G.plus
*Transpile> W.run_with q [10, 20]
30
```

Since the While Programs resulting from transpilation from Goto Programs are rather large and use helper registers, there is an extra pretty print function that first transpiles a Goto Program and then prints it prettified:

```
*Transpile> pretty_print_trans_g G.plus math_symbols2
Inc h;
While h {
  While c {
    Dec c;
    Inc c';
    Inc tmp;
  }
  While tmp {
    Dec tmp;
    Inc c;
  }
  IfZ c' {
    IfZ x {
      While c {
        Dec c;
      }
      Inc c;
      Inc c;
      Inc c;
      Inc c;
    } else {
      Inc c;
    }
  } else {
    Dec c';
    IfZ c' {
      Dec x;
      Inc c;
    } else {
      Dec c';
      IfZ c' {
        Inc n;
        Inc c;
      } else {
        Dec c';
        IfZ c' {
          While c {
            Dec c;
          }
        } else {
          Dec c';
          IfZ c' {
            IfZ y {
              While c {
                Dec c;
              }
              Inc c;
              Inc c;
              Inc c;
              Inc c;
              Inc c;
              Inc c;
              Inc c;
              Inc c;
            } else {
              Inc c;
            }
          } else {
            Dec c';
            IfZ c' {
              Dec y;
              Inc c;
            } else {
              Dec c';
              IfZ c' {
                Inc n;
                Inc c;
              } else {
                Dec c';
                IfZ c' {
                  While c {
                    Dec c;
                  }
                  Inc c;
                  Inc c;
                  Inc c;
                  Inc c;
                } else {
                  Dec c';
                  IfZ c' {
                    Dec h;
                  } else {
                    Dec c';
                    Skip;
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}
```
