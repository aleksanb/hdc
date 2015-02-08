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

# The d language

## Language features

### Macros

The emolicious platform doesn't support functions.
Instead, the d language provides compile-time macros.

An example macro invocation will look like the following.
Macro arguments are optional, result bindings required.

```result = @sin(x, y)```

Hdc will look for a file with the corresponding name, in this case 'sin.d'.
The contents will be parameterized with the provided macro arguments and result name.

### Implementing your own d macro

The macro interface is relatively simple to implement.

* All local variables must be prefixed with '__'. This to allow for easy namespacing by the compiler.
* Argument bindings must be on the form '__local_variable = __param<INDEX>', with a strictly increasing index for each parameter.
* A return binding, on the form '__return = '

A fully working macro with corresponding invocation is presented below.

```
$address_high = $id_high
$address_low = $id_low

$data = @sum(1000, 20)
store!
```


```
__left = __param0
__right = __param1

__return = __left + __right
```
