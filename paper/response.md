We thank the reviewers for their comments and suggestions.

# Key points

Referee B makes a single criticism of our paper:

> The emphasis of the minimality property over correctness seems somewhat off.

We agree that correctness is more important, but respectfully *disagree* that our
emphasis is on minimality. On the contrary, a larger part
of the paper is dedicated to the precise formulation of what it means for a
build system to be correct -- see S3.6 (correctness), S6.3 (correctness in
presence of non-determinism) and S6.4 (correctness of shallow cloud builds).

> The property most build system authors chase after is that an incremental
> build should be equivalent to a clean build, up to non-determinism

Yes indeed. That statement is a consequence of our defintion of correctness
on page 11.   (Our statement is a bit stronger, because it does not rely
on some earlier notion of a "clean build".   For example, presumably
a "clean build" means a build in which all inputs are treated as out
of date; and also that the results of that build are "correct".  But
what does "correct" mean?  Our definition avoids these difficulties.)

> B: How do these design decisions affect the developer experience?
> C: the Build abstraction itself is trivial and therefore does not help us understand some interesting aspects of the build systems.

Our abstractions are simple, but not *too* simple. They allow us to
identify and study a few key (but subtle) features, while carefully
abstracting away from the details (such as exploiting parallelism,
caching, and so on).  But for the developer experience, those
engineering details are super-important, and they absorb 90%+ of the
code that implements a build system.  wo while our abstractions allow
us to study, classify, compare, and contrast the landscape of build
systems, they abstract away too much to say much about the
developer experience.

# Details

## Reviewer A

> Presumably you could call `dependencies` on monadic tasks?

No, we cannot call `dependencies` on monadic tasks. Dependencies of a monadic
task cannot be determined without providing actual values; see e.g. line 576ff.

## Reviewer B

> reproducible/deterministic builds.

A strength of our abstractions is that they allow us to discuss the the tradeoffs
involved in requiring full determinism.  We did not do so in the submission, but
we will address the determinism/non-determinism tradeoff in our revision.

> npm, yarn, cabal, and cargo, etc?

Packaging systems like cabal or npm bring in a huge new collection of
considerations (such as constraint-solving to find consistent package sets)
which are way beyond our scope here.  Perhaps there is another similar paper
to be written about packaging systems!

> ML Compilation Manager?

Many languages have in-built build systems, including ML (as you mention),
Haskell's `ghc --make`, and OCamlMake. In the interests of space, and the
reader's bandwidth, and breadth of impact, we chose to focus on big, corpoprate-scale,
language-independent build systems, rather than language-specific ones.

## Reviewer C

> In the present form, the code of each build system is quite different from each other

In fact, our framework does provide reusable 'schedulers' (topological, recursive, etc.),
which are re-used in the definitions of build systems. One can also factor out
'rebuilders', e.g. based on verifying/constructive traces. We will address this
in the revision.


-------------------
> I can't see how the build system could distinguish this
> [static] dependency from those arising from parsing release.txt.

Indeed, Shake does not distinguish static dependencies from dynamic: it
treats them uniformly as if they all were dynamic.

> how does the file system metadata fit into this model? Is it persistent
> build information?

We consider file system metadata to be a part of persistent build information.
One could refine the model by introducing a separate notion for the file system
metadata, but we decided against this, since it makes the model more complex.

> this paper seems to suppose that the toplevel target is single and the changes
> are passively propagated. If this is true, this must be mentioned earlier.

Excel does not have the toplevel target, but one could introduce it by adding a
cell that depends on all other cells. We will clarify the correspondence between
real Excel and our model.

: for `sprsh2`, for
example, we need to know the value of the cell `C1` to determine the exact set
of dependencies; `track` obtains values using the `fetch :: k -> m v` argument.
