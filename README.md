# ImpCore
A toy implementation of a toy language.

More specifically, a Haskell implementation of the language ImpCore, which uses some nice, high-abstraction Haskell machinery and is, in my opinion, reasonably simple and well-coded.

Most of the operational semantics and much of the syntax taken from the language of the same name in Programming Languages: Build, Prove, and Compare by Norman Ramsey (which, as of this writing, is unpublished...)

## How to install and use
This package is built and managed with the Haskell build tool [Stack](https://docs.haskellstack.org/en/stable/README/). Apparently, Stack facilitates perfectly reproducible builds. However, [similar to Git](https://xkcd.com/1597/), I don't actually know how to use it. So fingers crossed.

So yeah, first [download Stack](https://docs.haskellstack.org/en/stable/README/). Next pull this Git project down. You'll need to run a couple of stack commands (in this project directory) next:

    # download the (ghc) haskell compiler
    stack setup
    stack build

I'm pretty sure the executable is now build, but it's kind of buried in Stack nonsense and I don't think it works without Stack's help anyway. So don't run the executable directly, instead try 

    stack exec ImpCore-exe -- --help
    stack exec ImpCore-exe -- --repl 
    stack exec ImpCore-exe -- test/t01.imp 4
    stack exec ImpCore-exe -- --repl test/t01.imp 4
    
(The `--` is just to say "stop interpreting arguments as arguments to `stack exec` and start passing them to the ImpCore-exe executable). There are three usages: interpreting a file (with arguments if you want), entering a read-eval-print loop, or doing both in one command (so you can play around with functions designed in a file). Protip: use rlwrap:

    rlwrap stack exec ImpCore-exe -- --repl test/t01.imp 4


## Things that are interesting about this implementation:

* All of the operational semantics of the language is implemented in around 100 lines of code (all in the file src/OpSem.hs). The code there reads very much like a formal specification in terms of judgements and derivations (provided that you know how to read basic Haskell/ML). 
  * It is technically a bit interesting in that it uses recursion schemes from [this library](https://hackage.haskell.org/package/recursion-schemes-5.0.1/docs/Data-Functor-Foldable.html). Almost all of the recursion needed to implement the language is very principled: you hand an environment to a sub-expression and get back a value and a new environment. All of needed recursion of this type is handled automatically by the recursion scheme, which (theoretically) provides easier to understand and maintain code (provided that you understand recursion schemes, which is really hard to understand to be fair. Sorry I don't know of a good resource on them).

* The files (and lines of code) in src/ break down like this:
  * AST.hs: define the AST and environment datatypes. 78 lines. This file is kinda interesting, and necessary to understand OpSem.hs.
  * OpSem.hs: define ALL of the operational semantics. 126 lines. Definitely the most interesting file. 
  * Parse.hs: parses... 84 lines. Not very interesting. Probably much longer than it has to be, considering how simple parsing is in this language, but I'm not very good at using parser combinators just yet.
  * CLI.hs: command line interface. 98 lines. Not interesting at all.
