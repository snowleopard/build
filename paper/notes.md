# Meeting with Simon Marlow

* Make:

  - Expand the description of the workaround used to deal with the lack of
    early cutoff. Explain why we chose not to model this (easier to just
    implement early cutoff).

* Buck:

  - Has `contents :: Map RuleKey v` instead of `Map (Hash v) v` where `RuleKey`
    is the hash of all inputs of a particular key. This relies on determinism:
    inputs fully determine the build outcome. If this assumption is violated,
    the result can be a frankenbuild (i.e. the executable will segfault).
  - Has no early cutoff. Impossible to support early cutoff using this approach.
  - Relies on applicative tasks to compute `RuleKey` from a key. How do we
    adapt this to dynamic dependencies?

# Feedback on the draft

## Graydon Hoare (Apple)

Nice paper! I suspect it's worth citing a couple steps upstream in redo's
lineage (Dan Bernstein, Alan Grosskurth). Might also be worth putting fbuild
(which works similarly) in the comparison: https://github.com/felix-lang/fbuild

## Yaron Minsky (Jane Street)

You might want to think about how something like this fits into your worldview:
https://github.com/grailbio/reflow
This is a build system for scientific workflows, courtesy of @marius.
I'm not sure it's really anything other than a cloud build system. Is it any
more distributed than Bazel? Also, from what I remember from chatting with
@marius about this, one difference in Reflow is the support of streaming, which
no other build system I know about supports.

(Response by @marius: I tend to think of it as somewhere between a build system
and a data processing system. Because it needs to deal with large data sizes
(often tens of terabytes) and long running computation, things like data
locality become a big concern. Another big difference is that it admits dynamic
graphs, so can express arbitrary computation e.g., you can express mapreduce and
flume/spark-like computation (albeit inefficiently) in Reflow. I think another
big difference between Reflow and traditional build systems is that it provides
a front end language that has lazy evaluation semantics. You write a program,
unreachable computation is implicitly pruned. There are no explicitly
declarations of dependencies.)

Also, if Excel is a build system, are self-adjusting computation libraries also
build systems?

I tend to think that SAC is about pure computation, and build systems have a
deadly embrace with external programs that you need to run as part of the
computation. It's this deadly embrace with a complex effectful system is what I
think of as the distinguishing factor.

Not a build system, but FrTime it's an incremental programming language where
the students are implicit rather than explicit. So there's some relation there.

I think my main suggestion for improvement for the paper is a fuller treatment
of SAC style approaches. If you're including Excel as a build system, it's hard
to see why SAC isn't fully in scope.

Another thought: another intellectual frame for what you're discussing (other
than build systems) is incremental computation. That's a much wider world, and
it would be good to have a clearer intellectual basis for understanding just
what is the paper's narrower scope.

Some of the issues you exclude as "pragmatics" are really where I would hang the
fundamental distinction. My gut is that these pragmatics are at the crux of the
distinction, and are depend questions than you suspect.

## Dan Bentley (worked on Google Sheets's calculation engine)
‏
I'll be honest, it irked me in describing Excel as a build system. I worked on
(but didn't write!) Google Sheets's calculation engine, and there's a lot not
covered. E.g., inserting a column can't rewrite every cell in the store.

Happy to chat more. I was surprised how much quad trees mattered for sheets
computation engine.

Dependencies are often over a contiguous range of cells. e.g. =SUM(A1:B23).
And there are enough they start getting memory intensive. So store them as
ranges, not independent deps per cell. Also helps with the insert-a-row case.

## Arseniy Alekseyev (Jane Street)

Nix should be in the Cloud Shake box.

## Ulan Degenbaev (Google)

How do I do configuration in this framework? Real-life example from Chromium:

https://cs.chromium.org/chromium/src/build/config/v8_target_cpu.gni?rcl=c8f117ce2885070675675564dc39be7e92c6853d&l=38

Andrey's response: What about the following? Here Bool is a configuration type.

staticIF :: Bool -> Task Applicative String Int
staticIF b fetch "B1" = Just $ if b then fetch "A1"
                                    else (+) <$> fetch "A2" <*> fetch "A3"
staticIF _ _     _    = Nothing

# Questions

* What about Turing-completeness? Shall the build tasks be Turing-complete?

# Recursion conjecture

Compute Functor = tail recursion
Compute Applicative = primitive recursion
Compute Monad = general recursion

# ApplicativeZero and MonadZero

These allow to introduce only failures into build systems.

As an interesting build system feature, one might have a build system that
performs a retry when a compute fails -- analogous to travis_retry. I found
an example of similar functionality in Bazel:

--flaky_test_attempts=<a positive integer, the string "default", or
test_regex@attempts. This flag may be passed more than once> multiple uses are
accumulated

Each test will be retried up to the specified number of times in case of any
test failure. Tests that required more than one attempt to pass would be marked
as 'FLAKY' in the test summary. If this option is set, it should specify an int
N or the string 'default'. If it's an int, then all tests will be run up to N
times. If it is not specified or its value is ' default', then only a single
test attempt will be made for regular tests and three for tests marked
explicitly as flaky by their rule (flaky=1 attribute).

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
