# Build Systems à la Carte

[![Hackage version](https://img.shields.io/hackage/v/build.svg?label=Hackage)](https://hackage.haskell.org/package/build) [![Linux & OS X status](https://img.shields.io/travis/snowleopard/build/master.svg?label=Linux%20%26%20OS%20X)](https://travis-ci.org/snowleopard/build) [![Windows status](https://img.shields.io/appveyor/ci/snowleopard/build/master.svg?label=Windows)](https://ci.appveyor.com/project/snowleopard/build)

This project provides an executable framework for developing and comparing build systems, viewing them as
related points in landscape rather than as isolated phenomena. The code derives from the ICFP 2018 paper
["Build Systems à la Carte"](https://github.com/snowleopard/build/releases/download/icfp-final/build-systems.pdf).


## Getting Started

You may be interested to:

* Run `stack test` to execute all the provided build systems on a very simple example.
* Look at the [HTML documentation](https://hackage.haskell.org/package/build) of all modules for the last release,
  or generate it yourself using `stack haddock`.
* Read the code, particularly [Build.System](src/Build/System.hs), which is the concrete implementation of
  all build systems.

## Related Material

* Blog post [on the motivation behind the project](https://blogs.ncl.ac.uk/andreymokhov/cloud-and-dynamic-builds/).
* Blog post [on the task abstraction](https://blogs.ncl.ac.uk/andreymokhov/cloud-and-dynamic-builds/).
* Blog post [reviewing how the paper was written](https://neilmitchell.blogspot.com/2018/07/inside-paper-build-systems-la-carte.html).
* Talk [by Neil on this paper and Shake](https://ndmitchell.com/#shake_18_may_2018).
