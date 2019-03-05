We thank the reviewers for their comments and suggestions.

# Key points

> **B:** The emphasis of the minimality property over correctness seems somewhat
off.

We agree that correctness is more important, but respectfully *disagree* that
our emphasis is on minimality. On the contrary, a larger part of the paper is
dedicated to the precise formulation of build system correctness (S3.6),
including non-deterministic (S6.3) and shallow build (S6.4) cases.

> **B:** The property most build system authors chase after is that an
incremental build should be equivalent to a clean build, up to non-determinism

Yes indeed. Our definition of correctness is stronger than "equivalent to a
clean build", because it does not rely on some earlier notion of a "clean build".
Presumably a "clean build" means a build in which all inputs are treated as
out-of-date; and *also* that the results of that build are "correct". But what
does "correct" mean? Our definition is self-contained.

> **B:** How do these design decisions affect the developer experience?

> **C:** the Build abstraction itself is trivial and therefore does not help
us understand some interesting aspects

Our abstractions are simple, but not *too* simple. They allow us to identify and
study a few key (but subtle) features, while carefully abstracting away from the
details (such as exploiting parallelism, caching, and so on). But for the
developer experience, those engineering details are super-important, and they
absorb 90%+ of the code that implements a build system. So while our
abstractions allow us to study, classify and compare build systems, they
abstract away too much to say much about the developer experience.

# Details

> **A:** I can't see how the build system could distinguish this
> [static] dependency from those arising from parsing `release.txt`.

Indeed, Shake does not distinguish static dependencies from dynamic: it treats
them uniformly as if they all were dynamic.

> **A:** Presumably you could call `dependencies` on monadic tasks?

No. Dependencies of a monadic task cannot be determined without providing actual
values; see the example in lines 576-585.

> **B:** reproducible/deterministic builds

A strength of our abstractions is that they allow us to discuss the trade-offs
involved in requiring full determinism. We will add such a discussion in our
revision.

> **B**: npm, yarn, cabal, and cargo, etc?

Packaging systems bring in a new collection of considerations (such as
constraint-solving to find consistent package sets) which are beyond our scope
here. Perhaps there is another similar paper to be written about packaging systems!

> **B:** ML Compilation Manager?

Many languages have built-in build systems, including ML (as you mention),
Haskell's `ghc --make`, and OCamlMake. In the interest of space and breadth of
impact, we chose to focus on language-independent build systems, rather than
language-specific ones.

> **C:** In the present form, the code of each build system is quite different
from each other

In fact, our framework does provide reusable 'schedulers' (`topological`,
`recursive`, etc.) and defines build systems using them. One can also factor out
'rebuilders', e.g. based on verifying/constructive traces. We will address this
in the revision.

The following didn't fit the limit:
-----------------------------------
> **A:** how does the file system metadata fit into this model? Is it persistent
> build information?

We consider file system metadata to be a part of persistent build information.
One could refine the model by introducing a separate notion for the file system
metadata, but we decided against this, since it makes the model more complex.
