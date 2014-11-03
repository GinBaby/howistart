My name is Chris, I teach Haskell to people that are new to programming and as well as long-time coders. Haskell is a general purpose programming language that is most useful to mere mortals.

I'd like to show you how to write a package in Haskell and how you could hypothetically upload it to [Hackage](hackage.haskell.org).


## Installing tools for writing Haskell code

The most popular compiler for Haskell is `GHC` and you use `Cabal` along-side `GHC` to manage projects and their dependencies. Packaging itself is part of `GHC` via `ghc-pkg`.

To get GHC and Cabal installed, use one of the following links:

* Windows: http://www.haskell.org/platform/windows.html

* OS X: http://new-www.haskell.org/downloads/osx

* Linux: http://new-www.haskell.org/downloads/linux

After you've finished the install instructions, `ghc`, `cabal`, and `ghci` should all be in your path. `ghci` is the REPL (read-eval-print loop) for Haskell, though as often as not, you'll use `cabal repl` to invoke a REPL that is aware of your project and its dependencies.


## What we're going to make

We're going to write a little csv parser for some baseball data. You can download the data from
[here](https://raw.githubusercontent.com/bitemyapp/csvtest/master/batting.csv). If
you want to download it via the terminal on a Unix-alike (Mac, Linux, BSD, etc) you can do so via:

```bash
$ curl -0 https://raw.githubusercontent.com/bitemyapp/csvtest/master/batting.csv
```

It should be about 2.3mb when it's all said and done.

## Getting your project started

First we're going to make our directory for our project wherever we tend to stash our work. If we're on a Unix-alike, that'll look something like:

```bash
$ mkdir bassbull
$ cd bassbull
```

Having done that, we're now going to use `Cabal`, our GHC Haskell dependency manager and build tool, to create some initial files for us. You have a couple options here. You can use the interactive helper or you can define everything non-interactively in one go.

To do it interactively:

```bash
$ cabal init
```

And the command I used to do it non-interactively (edit as appropriate for your project):

```bash
$ cabal init -n -l BSD3 --is-executable --language=Haskell2010 -u
bitemyapp.com -a 'Chris Allen' -c Data -s 'Churning some CSV data' -p bassbull
```

I'm also going to add the gitignore from Github's gitignore repository
plus some additions for Haskell so we don't accidentally check in
unnecessary build artifacts or other things inessential to the project.

```
dist
cabal-dev
*.o
*.hi
*.chi
*.chs.h
.virtualenv
.hpc
.hsenv
.cabal-sandbox/
cabal.sandbox.config
cabal.config
*.prof
*.hp
*.aux
```


## Project layout

There's not a prescribed project layout, but there are a few guidelines I would advise following.

One is that [Edward Kmett's lens library](https://github.com/ekmett/lens) is not only a fantastic library in its own right, but is also a great resource for people wanting to see how to structure a Haskell project, write and generate `Haddock` documentation, and organize your namespaces. Kmett's library follows [Hackage guidelines](http://hackage.haskell.org/packages/) on what namespaces and categories to use for his libraries.

There is an alternative namespacing pattern demonstrated by [Pipes, a streaming library](http://hackage.haskell.org/package/pipes). It uses a top-level eponymous namespace. For an example of another popular project you could also look at [Pandoc](https://github.com/jgm/pandoc/) for examples of how to organize non-trivial Haskell projects.

I'm going to layout my project like so:

```bash
$ tree
.
├── LICENSE
├── Setup.hs
├── cabal.sandbox.config
├── bassbull.cabal
├── src
│   ├── Main.hs
4 directories, 7 files
```

Ordinarily I'd structure things a little more, but there isn't a lot
to this project.

## Editing the Cabal file

We need to fix up our `cabal` file a bit. Mine is named `bassbull.cabal` and is in the top level directory of the project.

Before we start making changes, I'm going to init my version control (git, for me) so I can track my changes and not lose any work.

```bash
$ git init
$ git add .
$ git commit -am "Initial commit"
```

Here's what I changed my `cabal` file to:

```
name:                bassbull
version:             0.1.0.0
synopsis:            Processing some csv data
description:         Baseball data analysis
homepage:            bitemyapp.com
license:             BSD3
license-file:        LICENSE
author:              Chris Allen
maintainer:          cma@bitemyapp.com
copyright:           2014, Chris Allen
category:            Data
build-type:          Simple
cabal-version:       >=1.10

executable garrulous
  ghc-options:         -Wall -threaded
  hs-source-dirs:      src
  main-is:             Main.hs
  build-depends:       base >= 4.7 && <5,
                       bytestring,
                       vector,
                       cassava
  default-language:    Haskell2010
```

A few notable changes:

Set the description so Cabal would stop squawking about it.

Set `hs-source-dirs` to `src` so Cabal knows where my modules are.

Added a named executable stanza to the Cabal file so I can build a binary by that name and run it.

Set `main-is` to `Main.hs` in the executable stanza so the compiler knows what main function to use for that binary.

Set `ghc-options` to `-Wall` so I get the *rather* handy warnings GHC offers on top of the usual type checking.

Added `-threaded` to the `ghc-options` for the executable as we'll be taking advantage of threading later.

We included `garrulous` as a dependency for the executable stanza so that it can see the library.


## Building and interacting with your program

The contents of `src/Main.hs`:

```haskell
module Main where

main = putStrLn "hello"
```

One thing to note is that for a module to work as a `main-is` target for GHC, it must have a function named `main` and itself be named `Main`. Most people make little wrapper `Main` modules to satisfy this, sometimes with argument parsing and handling done via libraries like [optparse-applicative](https://github.com/pcapriotti/optparse-applicative).

For now, we've left Main to be very simple, making it just a
`putStrLn` of the string `"Hello"`. To validate that everything is
working, lets build and run this mostly pointless program.

First we create our Cabal sandbox so that our dependencies are
isolated to this project.

```bash
$ cabal sandbox init
```

Then we install our dependencies. These should get installed into the
Cabal sandbox package-db now that our sandbox has been created.
Otherwise they'd get installed into our user package-db located in
our home directory, which would be global to all the projects on our
current user account.

```bash
$ cabal install --only-dependencies
```

Now we're going to build our project.

```bash
$ cabal build
```

If this succeeds, we should get a binary named `bassbull` in
`dist/build/bassbull`. To run this, do the following.

```bash
$ ./dist/build/bassbull/bassbull
hello
$
```

If that worked, lets move onto writing a little csv processor.

## Writing a program to process csv data


One thing to note before we begin is that you can fire up a
project-aware Haskell REPL using `cabal repl`. The benefit of
doing so is that you can write and type-check code interactively
as you explore new and unfamiliar libraries or just to refresh
your memory about existing code.

You can do so by running it in your shell like so.

```bash
$ cabal repl
```

If you do, you should see a bunch of stuff about loading packages
installed for the project and then a `Prelude>` prompt.

```
[1 of 1] Compiling Main             ( Main.hs, interpreted )
Ok, modules loaded: Main.
Prelude>
```

Now we can load our `src/Main.hs` in the REPL.

```
Prelude> :load src/Main.hs
[1 of 1] Compiling Main             ( Main.hs, interpreted )
Ok, modules loaded: Main.
Prelude> main
hello
Prelude> 
```

Becoming comfortable with the REPL can be a serious boon to
productivity. There is editor integration for those that want it as
well.

Now we're going to update our `src/Main.hs`.

```haskell
module Main where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

-- a simple type alias for data
type BaseballStats = (BL.ByteString, Int, BL.ByteString, Int)

-- from cassava
import Data.Csv

main :: IO ()
main = do
  csvData <- BL.readFile "batting.csv"
  let v = decode NoHeader csvData :: Either String (V.Vector BaseballStats)
  let summed = fmap (V.foldr summer 0) v
  putStrLn $ "Total atBats was: " ++ (show summed)
  where summer (name, year :: Int, team, atBats :: Int) sum = sum + atBats
```

Lets break down this code a little.

First, we need to read in a file. We called the lazy `ByteString`
namespace `BL`. From that namespace we used `BL.readFile` which has
type `FilePath -> IO ByteString`. It returns ByteString wrapped in IO
because it returns a means of obtaining bytes which must be tagged
with IO, not just the data itself.

You can see [the type of `BL.readFile` here](http://hackage.haskell.org/package/bytestring-0.10.4.0/docs/Data-ByteString-Lazy.html#v:readFile).

We're binding over the `IO ByteString` that `BL.readFile "batting.csv"` returns.
`csvData` has type `ByteString` due to binding over `IO`.

```haskell
main :: IO ()
main = do
  csvData <- BL.readFile "batting.csv"
```

`v` is the type you see at the right with the type assignment operator ::
I'm assigning the type to dispatch the typeclass decode uses to
parse csv data. See more about [the typeclass cassava uses for parsing
csv data here](http://hackage.haskell.org/package/cassava-0.4.2.0/docs/Data-Csv.html#t:FromRecord).

In this case, because I defined a `type` alias of a tuple for my record, I get
my parsing code for free (already defined for tuples, `bytestring`, and
`Int`).

```haskell
  let v = decode NoHeader csvData :: Either String (V.Vector BaseballStats)
```

Using the record summing function in the bottom where
clause. First we fmap over the `Either String (V.Vector BaseballStats)`
this lets us apply `(V.foldr summer 0)` to `V.Vector BaseballStats`
We partially applied the `Vector` folding function `foldr` to the
summing function which is how we're folding and the number `0` so we
have a "start" value for the fold. Generally in Haskell we don't write
raw loops or recurse directly, we use higher order functions and
abstractions that wrap up and give names to common things programmers
do. One of those very common things is folding data.


```haskell
  let summed = fmap (V.foldr summer 0) v
```

Lastly we stringify the summed up count using `show`, then
concatenate that with a string to describe what we're printing,
then print the whole shebang using `putStrLn`. The `$` is just so
everything to the right of the `$` gets evaluated before whatever is
to the left. To see why I did that remove the `$` and build the
code. Alternatively, I could've used parenatheses in the usual fashion.

```haskell
  putStrLn $ "Total atBats was: " ++ (show summed)
```

This is the folding function we mentioned earlier. You can hang `where`
clauses off of functions which are a bit like `let` but they
come last. `Where` clauses are more common in Haskell than `let`, but
there's nothing wrong with using both.

Our folding function here takes two arguments, the tuple record (we'll
have many of those in the vector of records), and the sum of our data
so far.

```haskell
  where summer (name, year :: Int, team, atBats :: Int) sum = sum + atBats
```

## Building and running our csv parsing program

First we're going to rebuild the project.

```bash
$ cabal build
```

Then, assuming we have the `batting.csv` I mentioned earlier in our
current directory, we can run our program and get the results.

```bash
$ ./dist/build/bassbull/bassbull
Total atBats was: Right 4858210
$
```
