# Iterative calculation

https://gsuiteupdates.googleblog.com/2016/12/new-iterative-calculation-settings-and.html
https://i.stack.imgur.com/fozjd.png
http://www.k2e.com/tech-update/tips/433-tip-performing-iterative-calculations-in-excel

What about LaTeX build rules?

# Recursion conjecture

Compute Functor = tail recursion
Compute Applicative = primitive recursion
Compute Monad = general recursion

# ApplicativeZero and MonadZero

These allow to introduce only failures into build systems.

As an interesting build system feature, one might have a build system that
performs a retry when a compute fails -- analogous to travis_retry. I found
an example of similar functionality in Bazel:

--flaky_test_attempts=<a positive integer, the string "default", or test_regex@attempts. This flag may be passed more than once> multiple uses are accumulated

Each test will be retried up to the specified number of times in case of any test failure. Tests that required more than one attempt to pass would be marked as 'FLAKY' in the test summary. If this option is set, it should specify an int N or the string 'default'. If it's an int, then all tests will be run up to N times. If it is not specified or its value is ' default', then only a single test attempt will be made for regular tests and three for tests marked explicitly as flaky by their rule (flaky=1 attribute).

See https://docs.bazel.build/versions/master/command-line-reference.html

# MonadZero, MonadPlus, MonadOr

Zero -- for failure.
Or -- for choosing the first success.
Plus -- for picking any success, non-deterministically?

See https://wiki.haskell.org/MonadPlus_reform_proposal

Alternative seems to be similar to MonadOr in that it chooses the first one?

# Memoization and self-adjusting computation

"More specifically, we show an interesting duality between memoization and
change propagation: memoization performs poorly with deep changes (where
change propagation performs well) and performs well with shallow changes
(where change propagation performs poorly)."

From "A consistent semantics of self-adjusting computation" by Umut Acar et al.



# Some old text

Applicative build system can only accept an applicative compute, because they
rely on building the full dependency graph upfront, which is impossible with
dynamic dependencies.

We start by applicative build systems, such as the classic vanilla
\Make\footnote{There are numerous implementations of \Make and none comes with a
formal specification. In this paper we therefore use a simple but reasonable
approximation to a real \Make that you might find on your machine.}.

Make is correct but non-minimal. It is unusual from other build systems in that
it relies on \emph{timed values}, i.e. on a map \hs{age :: v -> Time}, as a
heuristic to decide whether a value is up-to-date.

What about Nix? John Ericson suggested in a blog comment that it may
be somewhat monadic, see:
\url{https://blogs.ncl.ac.uk/andreymokhov/cloud-and-dynamic-builds/\#comment-1849}.

As of writing, XXX is a correct but non-minimal monadic build system. Here is
an example where it does unnecessary computation:

\begin{minted}[frame=single]{text}
A1 = 10
A2 = MIN(A1, 4)
A3 = SQRT(A2)
\end{minted}

After performing the build, XXX computes \textsf{A2 = 4} and \textsf{A3 = 2}
from the input cell \textsf{A1}. Now if the user changes the \textsf{A1} to 5,
XXX will recompute both \textsf{A2 = 4}, which is necessary, but also
\textsf{A3 = 2}, which is unnecessary since its only dependency (\textsf{A1})
has not changed since the previous build.

One possible implementation of a spreadsheet build
system\footnote{For example, see \url{http://www.decisionmodels.com/calcsecretsc.htm}.}
is to maintain a
sequence of cells in which they should be evaluated. The build system takes this
sequence as input, marks all cells as \emph{unevaluated}, and then attempts to
evaluate all cells in the order specified by the sequence. If the cell to be
evaluated depends on an unevaluated cell, this means the provided sequence is
not a correct topological order and is updated by moving the current cell to the
back of the sequence. The build then proceeds with the next cell. Otherwise, if
all dependencies of the current cell have been marked as \emph{evaluated}, the
build system computes the correct value for the cell by evaluating the formula,
writes it into the spreadsheet and proceeds to the next cell in the sequence. If
there are no cyclic dependencies, this process eventually terminates with
correct results. The resulting sequence, which is guaranteed to respect the
topological order of dependencies is stored to be reused during the next build.

...

Interesting note: XXX is unusual in that it tracks changes in the compute,
i.e. if a user edits a formula, XXX will correctly rebuild results. Most build
systems, including \Shake, do not meet this requirement and a clean rebuild may
be necessary if the user edits build rules.

...

\subsection{Cloud build systems}

Codebase of large software projects comprises millions of lines of code spread
across thousands of files, each built independently by thousands of developers
on thousands of machines. A distributed cloud build system speeds up builds
dramatically (and saves energy) by transparently sharing build products among
developers.

\todo{AM}{Also mention YYY (developed by ZZZ), Buck (Facebook) and Pants
(Twitter et al). Buck is essentially the same. Are they essentially the same?}

\Bazel is a cloud build system developed by Google, which supports caching of
build results. To achieve that, it maintains two partial maps:

\begin{minted}{haskell}
type Cache =  Hash  -> Maybe v
type Known = [Hash] -> Maybe Hash
\end{minted}

\hs{Cache} is a conventional \emph{content-addressable store}, that can be used
to fetch a previously computed value given its hash.

\hs{Known} records a known outcome of a computation that took a list of values
as input (represented by their hashes) and produced a resulting value (also
represented by its hash). Now, a build system has the following two options to
recompute a value:

\begin{itemize}
    \item Call the compute function on the store containing all up-to-date
    dependencies and put the computed value to the store.
    \item Collect hashes of all up-to-date dependencies \hs{hs} and query the
    \hs{Known} map. If this computation has been performed before, the map will
    contain the hash \hs{h} of one valid result (recall that the compute function
    may be non-deterministic). It is now possible to lookup \hs{h} in the
    \hs{Cache} and if it contains a value \hs{v} it can be downloaded to the
    local store instead of running the compute.
\end{itemize}

...

\todo{AM}{Also describe the XYZ algorithm.}