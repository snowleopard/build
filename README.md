# Notes and experiments on build systems

The current work is to define the semantics of build systems. There are four main components:

* [Store](https://github.com/snowleopard/build-systems/blob/master/src/Development/Build/Store.hs)
* [Plan](https://github.com/snowleopard/build-systems/blob/master/src/Development/Build/Plan.hs)
* [Compute](https://github.com/snowleopard/build-systems/blob/master/src/Development/Build/Compute.hs)
* [Build](https://github.com/snowleopard/build-systems/blob/master/src/Development/Build.hs)

To learn more about the motivation behind the project, read
[this blog post](https://blogs.ncl.ac.uk/andreymokhov/cloud-and-dynamic-builds/).

## Basic notions

Below we define basic notions used in build systems and other similar domains,
for example, spreadsheets.

### Keys, values, hashes and store

*Keys* are used to distinguish *values*. In build systems keys are typically
filenames, e.g. `src/file.c`, whereas values are file contents (a C program source
code in this case). In spreadsheets keys are cell names, e.g. `A1`, and values
are numbers, text, etc. that are typically displayed inside cells.

It is convenient to assume that the value *store* is total, i.e. it contains a
value for every possible key. We therefore assume that the type of values is
capable of encoding values corresponding to non-existent files and empty cells.

We use a cryptographic *hash function* to for efficient tracking and sharing of
build results.

In our current implementation type variables `k` and `v` stand for the types of
keys and values -- see
[Store](https://github.com/snowleopard/build-systems/blob/master/src/Development/Build/Store.hs).

### Input, intermediate and output values

Some values are provided by the user as *input*. For example, `src/file.c` can be
edited by the user who relies on the build system to compile it into `obj/file.o`.
Similarly, the user can input `A1 = 5` and `B1 = 9` expecting the spreadsheet
to compute their sum in `C1`, i.e. `C1 = 14`.

In the above examples, `obj/file.o` and `C1` are *output* values.

In some situations we might also need the notion of *intermediate* values, which
are not interesting for the user but are produced in the process of turning
inputs into outputs. For example, the user might only be interested in the
executable `bin/file.exe` obtained by linking `obj/file.o` with standard
libraries, in which case `obj/file.o` can be considered an intermediate value.
