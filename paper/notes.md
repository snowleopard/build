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

