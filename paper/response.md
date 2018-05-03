We thank all reviewers for their comments and suggestions and will address them
in the revision. We also respond to some of them below, starting with the most
important.

Referee B (an expert) makes a single criticism of our paper:

> The emphasis of the minimality property over correctness seems somewhat off.

We agree that correctness is more important, but respectfully *disagree* that our 
emphasis is on minimality. On the contrary, a larger part
of the paper is dedicated to the precise formulation of what it means for a
build system to be correct -- see S3.6 (correctness), S6.3 (correctness in
presence of non-determinism) and S6.4 (correctness of shallow cloud builds).

> The property most build system authors chase after is that an incremental
> build should be equivalent to a clean build, up to non-determinism - if there
> is any in your build!

We fully agree that correctness must take precedence. Our definition of
correctness allows the designers of build systems to verify that even with
incrementality and non-determinism, their build systems produce results that are
equivalent to clean builds.

*** Simon says: what does this para mean?  Are you saying "yes, your statement
"an incremental build should be equivalent to a clean build" is what correctness
means, and is what our correctness property states?  Or what? ***


# Other more detailed points

## Reviewer A

> you refer to a "static" dependency on release.txt,
> but I can't see how the build system could distinguish this
> dependency from those arising from parsing release.txt.

Indeed, Shake does not distinguish static dependencies from dynamic: it
treats them uniformly as if they all were dynamic.

> how does the file system metadata fit into this model? Is it persistent
> build information?

We consider file system metadata to be a part of persistent build information.
One could refine the model by introducing a separate notion for the file system
metadata, but we decided against this, since it makes the model more complex.

> Presumably you could call `dependencies` on monadic tasks --- `Monad`s are
> `Applicative` after all --- but just wouldn't get any useful information back?

No, we cannot call `dependencies` on monadic tasks. Dependencies of a monadic
task cannot be determined without providing actual values: for `sprsh2`, for
example, we need to know the value of the cell `C1` to determine the exact set
of dependencies; `track` obtains values using the `fetch :: k -> m v` argument.

## Reviewer B

> most large projects are today moving towards reproducible/deterministic builds.

A strength of our abstractions is that they allow us to discuss the the tradeoffs
involved in requiring full determinism.  We did not do so in the submission, but
we will address the determinism/non-determinism tradeoff in our revision.

> How do these design decisions affect the developer experience?

Our abstractions are, by design, rather high level.  We identify and study a few key
(and rather subtle) features, while carefully abstracting away from the details
(such as exploiting parallelism, caching, and so on) .  
But for the developer experience, those engineering details are super-important,
and they absorb 90%+ of the code that implements a build system.
In short, our abstractions allow us to study, classify, compare, and contrast
the landscape of build systems, but they abstract away too much to say much
about the developer experience.

> Are there any similarly interesting points in the design space for more
> build+package systems?

Packaging systems like cabal or npm bring in a huge new collection of
considerations (such as constraint-solving to find consistent package sets)
which are way beyond our scope here.  Perhaps there is another similar paper
to be written about packaging systems!

> Are there any interesting insights from the ML Compilation Manager?

Many languages have in-built build systems, including ML (as you mention),
Haskell's `ghc --make`, and OCamlMake. In the interests of space, and the
reader's bandwidth, and breadth of impact, we chose to focus on big, corpoprate-scale,
language-independent build sytems, rather than language-specific ones.

## Reviewer C

> this paper seems to suppose that the toplevel target is single and the changes
> are passively propagated. If this is true, this must be mentioned earlier.

Excel does not have the toplevel target, but one could introduce it by adding a
cell that depends on all other cells. We will clarify the correspondence between
real Excel and our model.

> It would be more interesting if the framework could provide a set of building
> blocks of build systems. In the present form, the code of each build system is
> quite different from each other

The framework does provide reusable 'schedulers' (topological, recursive, etc.),
which are re-used in the definitions of build systems. One can also factor out
'rebuilders', e.g. based on verifying/constructive traces. We will address this
in the revision.

> the Build abstraction itself is trivial and therefore does not help us
> understand some interesting aspects of the build systems.

The whole point of the paper is to ignore many crucially important and
substantial engineering aspects of build systems and bring out more subtle
design choices. The Build abstraction is simple, but not *too* simple: it does
allow us to distinguish between many existing build systems in a meaningful way.
Adding more details to the Build abstraction, e.g. introducing parallelism,
would obscure the essence.
