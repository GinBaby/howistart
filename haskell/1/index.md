My name is Chris, I teach Haskell to people completely new to programming and long-timers alike. Haskell is a general purpose programming language that is especially useful to mere mortals.

I'd like to show you how to write a package in Haskell and how you could hypothetically upload it to [Hackage](hackage.haskell.org).

## Installing tools for writing Haskell code

The most popular compiler for Haskell is `GHC` and you use `Cabal` along-side `GHC` to manage projects and their dependencies. Packaging itself is part of `GHC` via `ghc-pkg`.

To get GHC and Cabal installed, use one of the following links:

* Windows: http://www.haskell.org/platform/windows.html

* OS X: http://new-www.haskell.org/downloads/osx

* Linux: http://new-www.haskell.org/downloads/linux

After you've finished the install instructions, `ghc`, `cabal`, and `ghci` should all be in your path. `ghci` is the REPL (read-eval-print loop) for Haskell, though as often as not, you'll use `cabal repl` to invoke a REPL that is aware of your project and its dependencies.

## Getting your project laid out

There's not a prescribed project layout, but there are a few guidelines I would advise following.

One is that [Edward Kmett's lens library](https://github.com/ekmett/lens) is not only a fantastic library in its own right, but is also a great resource for people wanting to see how to structure a Haskell project, write and generate `Haddock` documentation, and organize your namespaces. Kmett's library follows [Hackage guidelines](http://hackage.haskell.org/packages/) on what namespaces and categories to use for his libraries.

There is an alternative namespacing pattern demonstrated by [Pipes, a streaming library](http://hackage.haskell.org/package/pipes). It uses a top-level eponymous namespace.

## What we're going to make

We're going to make a chat server for a protocol that's a bit like IRC. One difference is that it will be text-only.
