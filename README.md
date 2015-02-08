hdc
===

Haskell Demolicious compiler

Stuff
====

- [Parsing a simple imperative language](https://www.haskell.org/haskellwiki/Parsing_a_simple_imperative_language)
- [Demolicious system](https://github.com/dmpro2014/)
- [Learn you a haskell](http://learnyouahaskell.com/chapters)

Thanks to
====
- [Haskell parser examples](https://github.com/ghulette/haskell-parser-examples)


Installation
===

You need to have the haskell platform to work with this project.
See this page for [Haskell Platform Download](https://www.haskell.org/platform/).

Consult this page for details cabal install tools [Haskell cabal download](https://www.haskell.org/cabal/download.html).
You should however already have this installed from the haskell platform.

### Mac OS X

If you are using OS X you should be using `homebrew`.
To download the Haskell platform you can write `brew install ghc cabal-install`.
Follow the instructions given under the installation.


Setting up your own environment
===

To set up your environment there are a few this that needs to be done.
Firstly set up your sandbox and install dependencies.
```
$ cabal sandbox init
$ cabal install alex
$ cabal install happy
$ cabal install pretty-show
```

You should be able to `make` and get a result now!


# D Language features

## Macros

The demolicious platform has no support for runtime functions.
Instead, the compiler has support for compile-time expansion of macros.
This keeps the source code dry, allowing code reuse through shared code fragments.

A macro invocation, ```result = @sin(x, y)```,  looks very much like a function call from other languages.

Current limitations include:

* No nesting of macro invocations
* Result has to be bound to a name

### Implementing your own d macro

The macro interface consists of the following

* To export a '@somename' macro, the d fragment must be named 'somename.d'.
* All local variables must be prefixed with '__'. This to allow for easy namespacing by the compiler.
* Optional argument bindings, on the form '__local_variable = __param<INDEX>', with a strictly increasing index for each parameter.
* Optional return binding, on the form '__return = '.

### A working macro implementation

**main.d**
```
$address_high = $id_high
$address_low = $id_low

$data = @sum(1000, 20)
store!
```

**sum.d**
```
__left = __param0
__right = __param1

__return = __left + __right
```
