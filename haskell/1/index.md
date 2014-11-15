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


We're going to write a little csv parser for some baseball data. I don't care a whit about baseball, but it was the best example of free data I could find.


## Getting your project started


First we're going to make our directory for our project wherever we tend to stash our work. If we're on a Unix-alike, that'll look something like:

```bash
$ mkdir bassbull
$ cd bassbull
```

Now we're going to download our test data now that we're inside the directory of our `bassbull` project.

You can download the data from [here](https://raw.githubusercontent.com/bitemyapp/csvtest/master/batting.csv). If you want to download it via the terminal on a Unix-alike (Mac, Linux, BSD, etc) you can do so via:

```bash
$ curl -0 https://raw.githubusercontent.com/bitemyapp/csvtest/master/batting.csv > batting.csv
```

It should be about 2.3 MB when it's all said and done.


Having done that, we're now going to use `Cabal`, our GHC Haskell dependency manager and build tool, to create some initial files for us. You have a couple options here. You can use the interactive helper or you can define everything non-interactively in one go.

To do it interactively:

```bash
$ cabal init
```

And the command I used to do it non-interactively (edit as appropriate for your project):

```bash
$ cabal init -n -l BSD3 --is-executable --language=Haskell2010 -u bitemyapp.com \
  -a 'Chris Allen' -c Data -s 'Processing some CSV data' -p bassbull
```


Before we start making changes, I'm going to init my version control (git, for me) so I can track my changes and not lose any work.

```bash
$ git init
$ git add .
$ git commit -am "Initial commit"
```

I'm also going to add the gitignore from Github's gitignore repository plus some additions for Haskell so we don't accidentally check in unnecessary build artifacts or other things inessential to the project.

This should go into a file named `.gitignore` at the top level of your
bassbull project.

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

You might be wondering why we're telling `git` to ignore something
called a "cabal sandbox". Cabal, unlike the package managers in other
language ecosystems, requires direct and transitive dependencies to
have compatible versions. For contrast, Maven will use the "closest"
version. To avoid packages having conflicts, Cabal introduced
sandboxes which let you do builds of your projects in a way that
doesn't use your user package-db. Your user package-db is global to
all your builds on your user account and this is almost never what you
want. This is not dissimilar from `virtualenv` in the Python
community. The `.cabal-sandbox` directory is where our build artifacts
will go when we build our project or test cases. We don't want to
version control that as it would bloat out the git repository and
doesn't need to be version controlled.


## Project layout


There's not a prescribed project layout, but there are a few guidelines I would advise following.

One is that [Edward Kmett's lens library](https://github.com/ekmett/lens) is not only a fantastic library in its own right, but is also a great resource for people wanting to see how to structure a Haskell project, write and generate `Haddock` documentation, and organize your namespaces. Kmett's library follows [Hackage guidelines](http://hackage.haskell.org/packages/) on what namespaces and categories to use for his libraries.

There is an alternative namespacing pattern demonstrated by [Pipes, a streaming library](http://hackage.haskell.org/package/pipes). It uses a top-level eponymous namespace. For an example of another popular project you could also look at [Pandoc](https://github.com/jgm/pandoc/) for examples of how to organize non-trivial Haskell projects.

Once we've finished laying out our project, it's going to look like this:

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


Here's where each of those files are going to come from.


* `cabal init` should have generated `LICENSE`, `Setup.hs`,
and `bassbull.cabal`.

* `cabal sandbox init` should have generated `cabal.sandbox.config`

* You'll need to `mkdir src` and `touch src/Main.hs` for your
`src/Main.hs` file to exist and that's where we'll be putting our
code.


Ordinarily I'd structure things a little more, but there isn't a lot to this project. Onward!


## Editing the Cabal file


First we need to fix up our `cabal` file a bit. Mine is named `bassbull.cabal` and is in the top level directory of the project.

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

executable bassbull
  ghc-options:         -Wall
  hs-source-dirs:      src
  main-is:             Main.hs
  build-depends:       base >= 4.7 && <5,
                       bytestring,
                       vector,
                       cassava
  default-language:    Haskell2010
```

A few notable changes:

* Set the description so Cabal would stop squawking about it.
* Set `hs-source-dirs` to `src` so Cabal knows where my modules are.
* Added a named executable stanza to the Cabal file so I can build a binary by that name and run it.
* Set `main-is` to `Main.hs` in the executable stanza so the compiler knows what main function to use for that binary.
* Set `ghc-options` to `-Wall` so we get the *rather* handy warnings GHC offers on top of the usual type checking.
* Added the libraries our project will use to `build-depends`.


## Building and interacting with your program


The contents of `src/Main.hs`:

```haskell
module Main where

main = putStrLn "hello"
```

One thing to note is that for a module to work as a `main-is` target for GHC, it must have a function named `main` and itself be named `Main`. Most people make little wrapper `Main` modules to satisfy this, sometimes with argument parsing and handling done via libraries like [optparse-applicative](https://github.com/pcapriotti/optparse-applicative).

For now, we've left Main to be very simple, making it just a `putStrLn` of the string `"Hello"`. To validate that everything is working, lets build and run this program.

First we create our Cabal sandbox so that our dependencies are isolated to this project.

```bash
$ cabal sandbox init
```

Then we install our dependencies. These should get installed into the
Cabal sandbox package-db now that our sandbox has been created.
Otherwise they'd get installed into our user package-db located in
our home directory, which would be global to all the projects on our
current user account. This can take some time on first run.

```bash
$ cabal install --only-dependencies
```

Now we're going to build our project.

```bash
$ cabal build
```

If this succeeds, we should get a binary named `bassbull` in `dist/build/bassbull`. To run this, do the following.

```bash
$ ./dist/build/bassbull/bassbull
hello
$
```

If that worked, lets move onto writing a little csv processor.

## Writing a program to process csv data


One thing to note before we begin is that you can fire up a project-aware Haskell REPL using `cabal repl`. The benefit of doing so is that you can write and type-check code interactively as you explore new and unfamiliar libraries or just to refresh your memory about existing code.

You can do so by running it in your shell like so.

```bash
$ cabal repl
```

If you do, you should see a bunch of stuff about loading packages
installed for the project and then a `Prelude>` prompt. -- it takes me straight to main at this point?

```
[1 of 1] Compiling Main             ( Main.hs, interpreted )
Ok, modules loaded: Main.
Prelude>
```

Now we can load our `src/Main.hs` in the REPL.

```
$ cabal repl
Preprocessing executable 'bassbull' for bassbull-0.1.0.0...
GHCi, version 7.8.3: http://www.haskell.org/ghc/  :? for help
Loading package ghc-prim ... linking ... done.
Loading package integer-gmp ... linking ... done.
Loading package base ... linking ... done.
Loading package array-0.5.0.0 ... linking ... done.
Loading package deepseq-1.3.0.2 ... linking ... done.
Loading package bytestring-0.10.4.0 ... linking ... done.
Loading package containers-0.5.5.1 ... linking ... done.
Loading package text-1.2.0.0 ... linking ... done.
Loading package hashable-1.2.2.0 ... linking ... done.
Loading package scientific-0.3.3.2 ... linking ... done.
Loading package attoparsec-0.12.1.2 ... linking ... done.
Loading package blaze-builder-0.3.3.4 ... linking ... done.
Loading package unordered-containers-0.2.5.1 ... linking ... done.
Loading package primitive-0.5.4.0 ... linking ... done.
Loading package vector-0.10.12.1 ... linking ... done.
Loading package cassava-0.4.2.0 ... linking ... done.
[1 of 1] Compiling Main             ( src/Main.hs, interpreted )

src/Main.hs:3:1: Warning:
    Top-level binding with no type signature: main :: IO ()
Ok, modules loaded: Main.
*Main> :load src/Main.hs
[1 of 1] Compiling Main             ( src/Main.hs, interpreted )

src/Main.hs:3:1: Warning:
    Top-level binding with no type signature: main :: IO ()
Ok, modules loaded: Main.
*Main>
```

Becoming comfortable with the REPL can be a serious boon to productivity. There is editor integration for those that want it as well.

Now we're going to update our `src/Main.hs`.

```haskell
module Main where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
-- from cassava
import Data.Csv

-- a simple type alias for data
type BaseballStats = (BL.ByteString, Int, BL.ByteString, Int)

main :: IO ()
main = do
  csvData <- BL.readFile "batting.csv"
  let v = decode NoHeader csvData :: Either String (V.Vector BaseballStats)
  let summed = fmap (V.foldr summer 0) v
  putStrLn $ "Total atBats was: " ++ (show summed)
  where summer (name, year, team, atBats) n = n + atBats
```

Lets break down this code.

```haskell
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
-- from cassava
import Data.Csv
```

First, we're importing our dependencies. Qualified imports let us give names to the namespaces we're importing and use those names as a prefix, such as `BL.ByteString`. This is used to refer to values and type constructors alike. In the case of `import Data.Csv` where we didn't qualify the import (with `qualified`), we're bringing everything from that module into scope. This should be done only with modules that have names of things that won't conflict with anything else. Other modules like `Data.ByteString` and `Data.Vector` have a bunch of functions that are named identically to functions in the `Prelude` and should be qualified.

```haskell
-- a simple type alias for data
type BaseballStats = (BL.ByteString, Int, BL.ByteString, Int)
```

Here we're creating a type alias for `BaseballStats`. I made it a type alias for a few reasons. One is so I could put off talking about algebraic data types! I made it a type alias of the 4-tuple specifically because the Cassava library already understands how to translate CSV rows into tuples and our type here will "just work" as long as the columns that we say are `Int` actually are parseable as integral numbers. Haskell tuples are allowed to have heterogenous types and are defined primarily by their length. The parentheses and commas are used to signify them. For example, `(a, b)` would be both a valid value and type constructor for referring to 2-tuples, `(a, b, c)` for 3-tuples, and so forth.

```haskell
main :: IO ()
main = do
  csvData <- BL.readFile "batting.csv"
```

We need to read in a file so we can parse our CSV data. We called the lazy `ByteString` namespace `BL` using the `qualified` keyword in the import. From that namespace we used `BL.readFile` which has type `FilePath -> IO ByteString`. You can read this out in English as `I take a FilePath as an argument and I return a ByteString after performing some side effects`. It returns ByteString wrapped in IO because it returns a means of obtaining bytes which must be tagged with IO, not the data directly without having first executed the side effects. --confusingly worded, didn't really understand this.

You can see [the type of `BL.readFile` here](http://hackage.haskell.org/package/bytestring-0.10.4.0/docs/Data-ByteString-Lazy.html#v:readFile).

We're binding over the `IO ByteString` that `BL.readFile "batting.csv"` returns. `csvData` has type `ByteString` due to binding over `IO`. Remember our tuples that we signified with parenthese earlier? Well, `()` is a sort of tuple too, but it's the 0-tuple! In Haskell we usually call it unit. It can't contain anything, it's a type that has a single value - `()`, that's it. It's often used to signify we don't return anything. Since there's usually no point in executing functions that don't return anything, `()` is often wrapped in `IO`. Printing strings are a good example of the result type `IO ()` as they do their work and return nothing. In Haskell you can't actually "return nothing", the concept doesn't even make sense, thus why we use `()` as the idiomatic "I got nothin' for ya" type & value. Usually if something returns `()` you won't even bother to bind to a name, you'll just ignore it.

```haskell
  let v = decode NoHeader csvData :: Either String (V.Vector BaseballStats)
```

`v` is the type you see at the right with the type assignment operator `::` I'm assigning the type to dispatch the typeclass that `decode` uses to parse csv data. See more about [the typeclass cassava uses for parsing csv data here](http://hackage.haskell.org/package/cassava-0.4.2.0/docs/Data-Csv.html#t:FromRecord).

In this case, because I defined a `type` alias of a tuple for my record, I get my parsing code for free (already defined for tuples, `bytestring`, and `Int`).


```haskell
  let summed = fmap (V.foldr summer 0) v
```

Here we're using a `let` expression to bind the expression `fmap
(V.foldr summer 0) v` to the name `summed` so that the expressions
that follow it can refer to `summed` without repeating all the same
code.

Using the record summing function in the bottom where clause. First we fmap over the `Either String (V.Vector BaseballStats)` this lets us apply `(V.foldr summer 0)` to `V.Vector BaseballStats`. We partially applied the `Vector` folding function `foldr` to the summing function and the number `0`. The number `0` here is our "start" value for the fold. Generally in Haskell we don't use recursion directly. Instead in Haskell we use higher order functions and abstractions, giving names to common things programmers do in a way that lets us be more productive. One of those very common things is folding data. You're going to see examples of folding and the use `fmap` from `Functor` in a bit.

We say `V.foldr` is partially applied because we haven't applied all of the arguments yet. Haskell has something called currying built into all functions by default which lets us avoid some tedious work that would require a "Builder" pattern in languages like Java. Unlike previous code samples, these examples are using my interactive `ghci` REPL.

```haskell
*Main> let appendOne = append [1]
*Main> appendOne [2, 3]

<interactive>:5:1: Warning:
    Defaulting the following constraint(s) to type ‘Integer’
      (Show a0) arising from a use of ‘print’ at <interactive>:5:1-16
      (Num a0) arising from a use of ‘it’ at <interactive>:5:1-16
    In a stmt of an interactive GHCi command: print it
[1,2,3]



-- first with list stuff
Prelude> let append x y = x ++ y
Prelude> :t append
append :: [a] -> [a] -> [a]
Prelude> let appendOne = append [1]
Prelude> appendOne [2, 3]
[1,2,3]
Prelude> appendOne []
[1]
Prelude> appendOne [4, 5, 6]
[1,4,5,6]

-- now with a product/record, if that
-- is confusing think "struct" but better.
Prelude> data Person = Person String Int String deriving Show

Prelude> :t Person
Person :: String -> Int -> String -> Person

Prelude> :t Person "Chris" 415
Person "Chris" 415 :: String -> Person

Prelude> :t Person "Chris" 415 "Allen"
Person "Chris" 415 "Allen" :: Person

Prelude> let namedChris = Person "Chris"
Prelude> namedChris 415 "Allen"
Person "Chris" 415 "Allen"

Prelude> Person "Chris" 415 "Allen"
Person "Chris" 415 "Allen"
```

This lets us apply some, but not all, of the arguments to a function and pass around the result as a function expecting the rest of the arguments.

Fully explaining the `fmap` in  `let summed = fmap (V.foldr summer 0) v`  would require explaining `Functor`. I don't want to belabor specific concepts *too* much, but I think a quick demonstration of `fmap` and `foldr` would help here. This is also a transcript from my interactive `ghci` REPL. I'll explain Either, Right, and Left after the REPL sample.


```haskell
Prelude> let v = Right 1 :: Either String Int
Prelude> let x = Left "blah" :: Either String Int

Prelude> :t v
v :: Either String Int
Prelude> :t x
x :: Either String Int

Prelude> let addOne x = x + 1
<interactive>:4:12: Warning:
    This binding for ‘x’ shadows the existing binding
      defined at <interactive>:3:5
Prelude> addOne 2
<interactive>:5:1: Warning:
    Defaulting the following constraint(s) to type ‘Integer’
      (Show a0) arising from a use of ‘print’ at <interactive>:5:1-8
      (Num a0) arising from a use of ‘it’ at <interactive>:5:1-8
    In a stmt of an interactive GHCi command: print it
3

Prelude> fmap addOne v
Right 2
Prelude> fmap addOne x
Left "blah"
```

`Either` in Haskell is used to signify cases where we might get values of one of two possible types. `Either String Int` is a way of saying, "you'll get either a `String` or an `Int`". This is an example of sum types, you can think of them as a way to say `or` in your type, where a `struct` or `class` would let you say `and`. `Either` has two constructors, `Right` and `Left`. Culturally in Haskell `Left` signifies an "error" case, this is partly why the `Functor` instance for `Either` maps over the `Right` constructor but not the `Left`. This is because if you have an error value, you can't keep applying your happy path functions. In the case of `Either String Int`, `String` would be our error value in a `Left` constructor and `Int` would be the happy-path "yep we're good" value in the `Right` constructor. Also, Haskell has type inference. You don't have to declare types explicitly like I did in the example from my REPL transcript - I did so for the sake of explicitness. The `:t` command is a command to my REPL, not part of the Haskell language. It's a way to request the type of an expression.

`Either` isn't the only type we can map over.

```haskell
Prelude> let myList = [1, 2, 3] :: [Int]
Prelude> fmap addOne myList
[2,3,4]
Prelude> let multTwo x = x * 2
Prelude> fmap multTwo myList
[2,4,6]
```

Here we have the list type, signified using the `[]` brackets and whatever type is inside in our list, in this case `Int`. With `Either` we have two possible types and `Functor` only lets us map over one of them, so the `Functor` instance for `Either` only applies our function over the happy path values. With `[a]` there's only one type inside of it, so it'll get applied regardless...or will it? What if I have an empty list?
-- wheres `[a]` coming from?

```haskell
Prelude> fmap multTwo []
[]
Prelude> fmap addOne []
[]
```

Conveniently not only does `fmap` let us avoid manually pattern matching the `Left` and `Right` cases of `Either`, but it lets us not bother to manually recurse our list or pattern-match the empty list case. This helps us prevent making mistakes as well as clean up and abstract our code. In a less happy alternate universe, we would've had to write the following code. Written in typical code file style rather than for the REPL this time.

```haskell
addOne :: Int -> Int
addOne x = x + 1 -- at least we can abstract this out

incrementEither :: Either e Int -> Either e Int
incrementEither (Right numberWeWanted) = Right (addOne numberWeWanted)
incrementEither (Left errorString) = Left errorString
```

We use parens on the left-hand side here to pattern match at the function declaration level on whether our `Either e Int` is `Right` or `Left`. Parentheses wrap `(addOne numberWeWanted)` are so we don't try to erroneously pass two arguments to `Right` when we mean to pass the result of applying `addOne` to `numberWeWanted`, to `Right`. If our value is `Right 1` this is returning `Right (addOne 1)` which reduces to `Right 2`.

As we process the CSV data we're going to be doing so by *folding* the
data. This is a general model for understanding how you process data
that extends beyond specific programming languages. You might have
seen `fold` called `reduce`. Here are some examples of folds and
list/string concatenation in Haskell. We're switching back to REPL
demonstration again.


```haskell
Prelude> :t foldr
foldr :: (a -> b -> b) -> b -> [a] -> b

Prelude> foldr (+) 0 [1, 2, 3]
6
Prelude> foldr (+) 1 [1, 2, 3]
7
Prelude> foldr (+) 2 [1, 2, 3]
8
Prelude> foldr (+) 2 [1, 2, 3, 4]
12

Prelude> :t (++)
(++) :: [a] -> [a] -> [a]

Prelude> [1, 2, 3] ++ [4, 5, 6]
[1,2,3,4,5,6]
Prelude> "hello, " ++ "world!"
"hello, world!"
```

Okay, enough of the REPL jazz session.

<hr>

Now back to the CSV processing code!

Lastly we stringify the summed up count using `show`, then concatenate
that with a string to describe what we're printing, then print the
whole shebang using `putStrLn`. The `$` is just so everything to the
right of the `$` gets evaluated before whatever is to the left. To see
why I did that remove the `$` and build the code. Alternatively, I
could've used parentheses in the usual fashion.

```haskell
  putStrLn $ "Total atBats was: " ++ (show summed)
```


To explain `show` from above, `show` is a function from the
typeclass `Show`. Here's how you can find out about it in your REPL.


```haskell
Prelude> :type show
show :: Show a => a -> String

Prelude> :info Show
class Show a where
  showsPrec :: Int -> a -> ShowS
  show :: a -> String
  showList :: [a] -> ShowS
-- Defined in ‘GHC.Show’
instance (Show a, Show b) => Show (Either a b)
-- Defined in ‘Data.Either’
instance Show a => Show [a] -- Defined in ‘GHC.Show’
instance Show Ordering -- Defined in ‘GHC.Show’
instance Show a => Show (Maybe a) -- Defined in ‘GHC.Show’
instance Show Integer -- Defined in ‘GHC.Show’
instance Show Int -- Defined in ‘GHC.Show’
instance Show Char -- Defined in ‘GHC.Show’
instance Show Bool -- Defined in ‘GHC.Show’
...
```

What `instance Show Integer` is telling us, is that `Integer` has
implemented `Show`. This means we should be able to use `show` on
something with that type. We can specialize the type of `show` to
`Integer` in a few passes.

```haskell
show :: Show a => a -> String
show :: Show Integer => Integer -> String
-- you can just drop Show Integer =>, the typeclass
-- instances associated with a specific type are 
-- a given.
show :: Integer -> String
```

In fact, we can even make a pointless version of show pre-specialized
to `Integer`. Here's an example from my REPL:

```haskell
Prelude> :t show
show :: Show a => a -> String
Prelude> :t show myInteger
show myInteger :: String
Prelude> let integerShow = show :: Integer -> String
Prelude> integerShow 1
"1"
Prelude> integerShow ("blah", ())

<interactive>:11:13:
    Couldn't match expected type ‘Integer’
                with actual type ‘([Char], ())’
    In the first argument of ‘integerShow’, namely ‘("blah", ())’
    In the expression: integerShow ("blah", ())
    In an equation for ‘it’: it = integerShow ("blah", ())
Prelude> show ("blah", ())
"(\"blah\",())"
```


Next we'll look at `summer`. `summer` is the function we are folding our `Vector` with. You can hang `where` clauses off of functions which are a bit like `let` but they come last. `where` clauses are more common in Haskell than `let` clauses, but there's nothing wrong with using both.

Our folding function here takes two arguments, the tuple record (we'll have many of those in the vector of records), and the sum of our data so far.

Here `n` is the sum we're carrying along as fold the `Vector` of `BaseballStats`.

```haskell
  where summer (name, year, team, atBats) n = n + atBats
```


## Building and running our csv parsing program


First we're going to rebuild the project.

```bash
$ cabal build
```

Then, assuming we have the `batting.csv` I mentioned earlier in our current directory, we can run our program and get the results.

```bash
$ ./dist/build/bassbull/bassbull
Total atBats was: Right 4858210
$
```


## Refactoring our code a bit

Splitting out logic into independent functions is a common method for making Haskell code more composable and easy to read.

To that end, we'll clean up our example a bit.

First, we don't care about `name`, `year`, and `team` for our folding code.

So we're going to use the Haskell idiom of bindings things we don't care about to `_`.

This changes our fold from this:

```haskell
where summer (name, year, team, atBats) sum = sum + atBats
```

To this:

```haskell
where summer (_, _, _, atBats) sum = sum + atBats
```

Next we'll make our extraction of the 'at bats' from the tuple more compositional. If you'd like to play with this further, consider rewriting our example program at the end of this article into using a Haskell record instead of a tuple. I used a tuple here because Cassava already understands how to parse them, sparing me having to write that code.
-- Who is this for? If this is my first Haskell program then I have no idea what a Haskell record is or how I'd do this.

First we'll add `fourth`:

```haskell
fourth :: (a, b, c, d) -> d
fourth (_, _, _, d) = d
```

Then we'll rewrite our folding function again from:

```haskell
where summer (_, _, _, atBats) n = n + atBats
```

Into:

```haskell
where summer r n = n + fourth r
```

Here we can use something called *eta reduction* to remove the explicit record and sum values to make it point-free. Since our function is really just about composing the extraction of the fourth value from the tuple and summing that value with the summed up `atBat` values so far, this makes the code quite concise.
-- This should be higher up because you've already mentioned eta reduction

You can read more about this in [the article on pointfree programming in Haskell](https://www.haskell.org/haskellwiki/Pointfree).

To that end, we go from:

```haskell
where summer r n = n + fourth r
```

to:

```haskell
where summer = (+) . fourth
```

First `fourth` gets applied to the `r` argument, then `(+)` is composed so that it is applied to the result of `fourth r` and the value `n`.
-- What does the . do?

We should also split out our decoding of `BaseballStats` from CSV data.

We're going to move this code:

```haskell
let v = decode NoHeader csvData :: Either String (V.Vector BaseballStats)
```

Into an independent function:

```haskell
baseballStats :: BL.ByteString -> Either String (V.Vector BaseballStats)
baseballStats = decode NoHeader
```

-- worth explaining here that we've curried baseballStats

Then `summed` becomes:

```haskell
let summed = fmap (V.foldr summer 0) (baseballStats csvData)
```

With that bit of tidying done, we should have:

```haskell
module Main where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
-- cassava
import Data.Csv

type BaseballStats = (BL.ByteString, Int, BL.ByteString, Int)

fourth :: (a, b, c, d) -> d
fourth (_, _, _, d) = d

baseballStats :: BL.ByteString -> Either String (V.Vector BaseballStats)
baseballStats = decode NoHeader

main :: IO ()
main = do
  csvData <- BL.readFile "batting.csv"
  let summed = fmap (V.foldr summer 0) (baseballStats csvData)
  putStrLn $ "Total atBats was: " ++ (show summed)
  where summer = (+) . fourth
```

Now we're going to double-check that our code is working:

```bash
$ cabal build
...(stuff happens)...
$ ./dist/build/bassbull/bassbull
Total atBats was: Right 4858210
```

## Streaming


We can improve upon what we have here. Currently we're going to use as much memory as it takes to store the entirety of the csv file in memory, but we don't really have to do that to sum up the records!

Since we're just adding the current records' "at bats" with the sum we've accumulated so far, we only really need to read one record into memory at a time. By default Cassava will load the csv into a `Vector` for convenience, but fortunately it has a streaming module so we can stream the data incrementally and fold our result without loading the entire dataset at once.

First, we're going to drop Cassava's default module for the streaming module.

Changing from this:

```haskell
-- cassava 
import Data.Csv
```

To this:

```haskell
-- cassava 
import Data.Csv.Streaming
```

Next, since we won't have a `Vector` anymore (we're streaming, not using in-memory collections), we can drop:

```haskell
import qualified Data.Vector as V
```

In favor using the `Foldable` typeclass Cassava offers for use with its streaming API:

```haskell
import qualified Data.Foldable as F
```

Then in order to use the streaming API we just change the definition of our `summed` from:

```haskell
let summed = fmap (V.foldr summer 0) (baseballStats csvData)
```

To:

```haskell
let summed = F.foldr summer 0 (baseballStats csvData)
```

We are incrementally processing the results, not loading the entire
dataset into a Vector.

The final result should look like:

```haskell
module Main where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Foldable as F
-- cassava
import Data.Csv.Streaming

type BaseballStats = (BL.ByteString, Int, BL.ByteString, Int)

fourth :: (a, b, c, d) -> d
fourth (_, _, _, d) = d

baseballStats :: BL.ByteString -> Records BaseballStats
baseballStats = decode NoHeader

main :: IO ()
main = do
  csvData <- BL.readFile "batting.csv"
  let summed = F.foldr summer 0 (baseballStats csvData)
  putStrLn $ "Total atBats was: " ++ (show summed)
  where summer = (+) . fourth
```

Because Haskell has abstractions like the `Foldable` typeclass, we can talk about folding a dataset without caring about the underlying implementation! We could've used the `foldr` from `Foldable` on our `Vector`, a `List`, a `Tree`, a `Map` - not just Cassava's streaming API. `foldr` from `Foldable` has the type: `Foldable t => (a -> b -> b) -> b -> t a -> b`. Note the similarity with the `foldr` for the list type, `(a -> b -> b) -> b -> [a] -> b`. What we've done is abstracted the specific type out and made it into a generic interface.


## Wrapping up


This is the end of our little journey in playing around with Haskell to process CSV data. Learning how to use abstractions like `Foldable`, `Functor` or use techniques like *eta reduction* takes practice! I have [a guide](https://github.com/bitemyapp/learnhaskell) for learning Haskell which has been compiled based on my experiences learning and teaching Haskell with many people over the last year or so.


If you are curious and want to learn more, I strongly recommend you do a course of basic exercises first and then explore the way Haskell enables you think about your programs in terms of abstractions. Once you have the basics down, this can be done in a variety of ways. Some people like to attack practical problems, some like to follow along with white papers, some like to hammer out abstractions from scratch in focused exercises & examples.


Things to do after finishing this article:

- [Check out the Haskell community website](https://haskell.org)
- [Learn about (unit|spec|property) testing Haskell software with Kazu Yamamoto's tutorial](https://github.com/kazu-yamamoto/unit-test-example/blob/master/markdown/en/tutorial.md)
- [Search for code by *type* structurally with Hoogle](http://haskell.org/hoogle)
- [Learn about Haddock, the Haskell source documentation tool](https://www.haskell.org/haddock/) and look at the many [examples](http://hackage.haskell.org/package/base-4.7.0.1/docs/Data-Functor.html) of Haskell package [documentation](http://hackage.haskell.org/package/pipes-4.1.3/docs/Pipes-Tutorial.html).


More than anything else, my greatest wish would be that you develop a richer and more rewarding relationship with learning. Haskell has been a big part of this in my life.


Special thanks to [Daniel Compton](https://twitter.com/danielwithmusic) and
[Julie Moronuki](https://twitter.com/argumatronic) for helping me test
& edit this article. I couldn't have gotten it together without their help.
