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

We're going to make a chat server for a protocol that's a bit like IRC. One difference is that it will be text-only. I'm going to name mine garrulous, but you can use whatever you want.


## Getting your project started

First we're going to make our directory for our project wherever we tend to stash our work. If we're on a Unix-alike, that'll look something like:

```bash
$ mkdir garrulous
$ cd garrulous
```

Having done that, we're now going to use `Cabal`, our GHC Haskell dependency manager and build tool, to create some initial files for us. You have a couple options here. You can use the interactive helper or you can define everything non-interactively in one go.

To do it interactively:

```bash
$ cabal init
```

And the command I used to do it non-interactively (edit as appropriate for your project):

```bash
$ cabal init -n -l MIT --is-library --language=Haskell2010 -u bitemyapp.com -a 'Chris Allen' -c Network -s 'A chat service for friends' -p garrulous
```

I'm also going to add the gitignore from Github's gitignore repository for Haskell so we don't accidentally check in unnecessary build artifacts or other things inessential to the project.

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
```

## Project layout

There's not a prescribed project layout, but there are a few guidelines I would advise following.

One is that [Edward Kmett's lens library](https://github.com/ekmett/lens) is not only a fantastic library in its own right, but is also a great resource for people wanting to see how to structure a Haskell project, write and generate `Haddock` documentation, and organize your namespaces. Kmett's library follows [Hackage guidelines](http://hackage.haskell.org/packages/) on what namespaces and categories to use for his libraries.

There is an alternative namespacing pattern demonstrated by [Pipes, a streaming library](http://hackage.haskell.org/package/pipes). It uses a top-level eponymous namespace.

I'm going to follow Kmett's lead and layout my project like so:

```bash
$ tree
.
├── LICENSE
├── Setup.hs
├── garrulous.cabal
├── src
│   └── Network
│       └── Chat
│           ├── Garrulous
│           └── Garrulous.hs
└── tests
    └── tests.hs
5 directories, 4 files
```

## Editing the Cabal file

We need to fix up our `cabal` file a bit. Mine is named `garrulous.cabal` and is in the top level directory of the project.

Before we start making changes, I'm going to init my version control (git, for me) so I can track my changes and not lose any work.

```bash
$ git init
$ git add .
$ git commit -am "Initial commit"
```

Here's what I changed my `cabal` file to:

```
name:                garrulous
version:             0.1.0.0
synopsis:            A chat service for friends
description:         Standalone chat server
homepage:            bitemyapp.com
license:             MIT
license-file:        LICENSE
author:              Chris Allen
maintainer:          cma@bitemyapp.com
copyright:           2014, Chris Allen
category:            Network
build-type:          Simple
cabal-version:       >=1.10

library
  hs-source-dirs:      src
  exposed-modules:     Network.Chat.Garrulous
  main-is:             Network.Chat.Garrulous
  build-depends:       base >=4.7 && <4.8
  default-language:    Haskell2010
```

A few notable changes:

Set the description so Cabal would stop squawking about it.

Set `hs-source-dirs` to `src` so Cabal knows where my modules are.

Set `main-is` to `Network.Chat.Garrulous` so the compiler knows what function to call when the program starts.
