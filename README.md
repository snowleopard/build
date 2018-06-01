# Build Systems à la Carte

[![Hackage version](https://img.shields.io/hackage/v/build.svg?label=Hackage)](https://hackage.haskell.org/package/build) [![Linux & OS X status](https://img.shields.io/travis/snowleopard/build/master.svg?label=Linux%20%26%20OS%20X)](https://travis-ci.org/snowleopard/build) [![Windows status](https://img.shields.io/appveyor/ci/snowleopard/build/master.svg?label=Windows)](https://ci.appveyor.com/project/snowleopard/build)

This project provides an executable framework for developing and comparing build systems, viewing them as
related points in landscape rather than as isolated phenomena. The code derives from the ICFP 2018 paper
["Build Systems à la Carte"](https://github.com/snowleopard/build-systems/releases/download/icfp-submission/build-systems.pdf).

## Getting Started

You may be interested to:

* Run `stack test` to execute all the provided build systems on a very simple example.
* Run `stack haddock` to generate HTML documentation of all the interfaces.
* Read the code, particularly [System.hs](src/Build/System.hs) which is the concrete implementation of
  all build systems. Following the imports (or the
  [Haddock documentation](https://hackage.haskell.org/package/build)) will lead you to all the
  constituent parts.

## Further Activities

There aren't really any. The code served as a proving ground for ideas, and its existence both allows
confirmation that our conclusions are valid, and opportunity to cheaply conduct further experiments. Although
the code is a useful adjoint to the paper, it is not essential to it (other than we wouldn't have been
able to discover what we did without an executable specification).

## Background Information

The task abstraction is explored more completely in
[this blog post](https://blogs.ncl.ac.uk/andreymokhov/the-task-abstraction/), and the motivation behind
the project in [an earlier blog post](https://blogs.ncl.ac.uk/andreymokhov/cloud-and-dynamic-builds/).
