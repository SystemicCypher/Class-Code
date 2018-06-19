# Assignment Four

This assignment is due March 8th at 2:00 PM.


## The Task

Write a type checker for our "Simply Typed Lambda Calculus" mini-language.

You will be  provided with parser code that  reads  STLC programs  from standard
input  and returns them in the form of an  abstract syntax tree.  Your job is to
implement a function `typeOf` that takes an AST (aka Expr) and an environment (a
map of variable names to types) and returns the type of that AST.

All input and  output is handled for you in the provided main functions.  If you
add  print statements for debugging, don't forget to remove them before you turn
in your code.

If the AST is well-typed, your `typeOf` function should return the correct type.
This will be printed to the console  automatically  (the exact output may differ
slightly between languages; that's okay).

If  the AST is _not_ well-typed, signal an error  (see language-specific details
below for how to do this).  Your error message will be automatically  printed to
the consle as `Type error: $YOURMESSAGE`.  When grading,  we will only check for
the `Type error: ` prefix - use the message to say something helpful.

If the  program is not well-formed,  the  parser will  fail and  you will  see a
`Syntax error: $SOMEMESSAGE`  message.  We will not give  you invalid input, but
you may see this if you enter an unparseable program when debugging.


## Examples

This is the Haskell output for some common cases.  More examples can be found in
the `Examples` folder (separated into directories of well- and ill-typed ASTs).

```
[01:56:44 holt@Minerva hw4]$ echo '\x:nat. x' | ./Haskell/Main
TFun TNat TNat
[01:57:06 holt@Minerva hw4]$ echo 'fst(5)' | ./Haskell/Main
Type error: NOPE
[01:57:24 holt@Minerva hw4]$ echo '\x.x' | ./Haskell/Main
Syntax error: (line 1, column 3):
unexpected "."
expecting letter, white space or ":"
```


## The Languages

You get a choice between Haskell and Scala.  The parsers for both  languages are
included in the starter code - just delete the folder for the language you don't
use.


### Haskell

Implement  the `typeOf` function at the bottom of the `Typechecker.hs` file.  If
given a well-typed AST, return the correct type as a Right value.  The available
types can be  found in the  `ExprType`  section of `Ast.hs`.  If the AST is  not
well-typed, return an error string as a Left value.

Build  your program using  `ghc`,  which installs as part of the default Haskell
installation.  You can then run it from the command line:

```
[01:18:52 holt@Minerva hw4]$ ghc --make Haskell/*.hs
[01:19:56 holt@Minerva hw4]$ ./Haskell/Main
hi
Main: Not implemented
```


### Scala

Implement the  `typeOf`  function at the bottom of the `TypeChecker.scala` file.
If given a well-typed AST,  return the correct `ExprType` as found at the top of
`Parser.scala`.  If the AST is not well-typed, throw a `TypeException`.

Build and run your program using `sbt` (which is an additional install):

```
[01:24:56 holt@Minerva hw4]$ cd Scala
[01:24:58 holt@Minerva Scala]$ sbt run
[info] Loading project definition from /path/to/cs162/hw4/Scala/project
~snip~ ...
[info] Running Assignment4.Main
hi
[error] (run-main-0) scala.NotImplementedError: an implementation is missing
[error] scala.NotImplementedError: an implementation is missing
[error]   at scala.Predef$.$qmark$qmark$qmark(Predef.scala:284)
~snip~    at ...
[error] (Compile / run) Nonzero exit code: 1
[error] Total time: 10 s, completed Feb 28, 2018, 1:25:16 AM
```

This is kinda slow.  Sorry.  We  may update this section with a faster,  quieter
way of running Scala code in a little bit.

