## The Cognate programming language
Cognate is a stack oriented, functional programming language designed to help programmers specify their intent. Cognate uses optional syntax in order to help programmers write statements that are (mostly) readable in plain English and to include extra information about what statements are for, not just what they do and how they work. This leads to more maintainable codebases. Cognate's syntax is much simpler than other languages that aim for readability such as python, making the language consistent and simple to implement. This repository includes CognaC - the Cognate Compiler.

![Program to compute the 42nd Fibonacci number](fibonacci.png?raw=true)

### Cognate is readable
As the above program shows, words starting with lowercase letters are ignored by the compiler (and are not highlighted). This allows comments and code to be easily bundled together, enhancing readability and allowing programmers to more easily express their intentions. While this means that Cognate can be very verbose, verbosity can be a good thing - this "informal syntax" gives extra information to anyone reading the source.

### Cognate is fast
Cognate is compiled directly to C. Even my naive attempt at a compiler is able to outperform most interpreted languages, leaving python and ruby in the dust. As time goes on, I will be able to optimise the compiler and gain even more performance. Furthermore, cognate produces very small binaries and could be used for a high level language in an embedded environment.

### Cognate is simple
Cognate has only a handful of syntax elements. In future, this will allow me to elegantly introduce compile time macros and reflection, similar to those found in lisp dialects, that manipulate the syntax tree directly. I am also looking at implementing Forth-style code metaprogramming with immediate words, etc. 
### Installing Cognate
Currently, Cognate is not in a particularly usable state, and thus, you probably don't want to install it. Cognate currently does not have a build system, but is not too hard to install on unix-based systems:
1) Run ```git clone https://github.com/stavromuellerbeta/cognate```.
2) Run ```cd cognate```.
3) Install the GHC haskell compiler and cabal-install through your distrubutions's package manager.
4) Run ```cabal update```.
5) Run ```cabal install MissingH split --lib```.
6) Compile CognaC with ```ghc cognac```.
7) CognaC is installed - run it with ```./cognac examples/fib.cog```
