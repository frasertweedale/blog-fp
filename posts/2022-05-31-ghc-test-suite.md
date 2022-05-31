---
tags: ghc, testing
---

# Writing tests for GHC

In this post I explain how to write functional tests for GHC, with
examples.

## Overview of the GHC test suite

The *Glasgow Haskell Compiler (GHC)* is a huge project.  It
includes:

- a Haskell compiler (parser, simplifier, codegen, etc)
- the runtime system (GC, thread scheduler, STM, etc)
- GHCi (interactive interpreter / REPL)
- bundled libraries (*base*, *template-haskell*, *ghc-prim*, etc)
- build tooling (Makefiles, [Hadrian][])
- the users guide
- a test suite

[Hadrian]: https://gitlab.haskell.org/ghc/ghc/-/wikis/building/hadrian

The test suite should test all the functional parts of GHC.  There
are different kinds of tests:

- **Testing the compiler.**  The test suite includes Haskell program
  sources.  A test can assert that the program compiles, or that
  compilation failure is expected.

- **Testing the resulting programs.**  Does the behaviour of a
  program match expectations?

- **Testing the bundled libraries.**  This is conceptually distinct
  from the preceding point.  In practice it can be achieved in
  the same way.

- **Performance tests** ensure that the performance of the
  compiler, and of compiled programs, does not regress.  This is a
  complex topic and I won't discuss it further in this post.  The
  GHC wiki has [a good introduction][performance-tests].

[performance-tests]: https://gitlab.haskell.org/ghc/ghc/-/wikis/building/running-tests/performance-tests

The GHC GitLab instances runs a **continuous integration (CI)**
pipeline for all merge requests.  It builds GHC and runs the test
suite on a variety of target architectures and operating systems.
Here's an [example for one of my merge requests][pipeline].

[pipeline]: https://gitlab.haskell.org/frasertweedale/ghc/-/pipelines/51652

The test suite driver is implemented in Python.  Tests are described
in terms of its library interface.  I won't discuss the
*implementation* of the test driver—I just want to show you how to
use it.

## Writing tests

In the GHC source repository, tests are organised heirarchically
under `testsuite/tests/`.  Bundled libraries can also supply tests.
For example, some tests for *base* sit under
`libraries/base/tests/`.  `.T` files describe the tests, using the
Python DSL.  Usually the name `all.T` is used.  The source code for
each test lives alongside the `.T` file.

### The test driver DSL

Each test is described in a `.T` file by an invocation of the `test`
function:

```python
test(<name>, <setup>, <test-fn>, <args>)
```

- `<name>` is the **name of the test** source file (without file
  extension), or the directory containing the source code (for
  multi-module builds).  Often, the name is based on an issue
  number.

- `<setup>` is a function or list of functions that affect **when or
  how to run the test program**.  For example, you can set extra
  program arguments, declare the expected exit status, or supply a
  predicate for skipping the test.  The GHC wiki gives a [full
  list][setup-field] with descriptions.

- `<test-fn>` specifies **how to compile** the test program, and
  **whether to run** the resulting program.  Common values include
  `compile`, `compile_fail` (compilation failure expected) and
  `compile_and_run` (run the resulting program).  There are [several
  other options][test-fn-field] such as for multi-module builds, and
  GHCi sessions.

- `<args>` specifies extra **arguments for GHC** when compiling the
  test program.  It also has other use patterns depending on the
  value of `<test-fn>`.

[setup-field]: https://gitlab.haskell.org/ghc/ghc/-/wikis/building/running-tests/adding#the-setup-field
[test-fn-field]: https://gitlab.haskell.org/ghc/ghc/-/wikis/building/running-tests/adding#the-test-fn-field

Alongside the source file (`<name>.hs`) are optional files for
specifying the input and output of the test program:

- `<name>.stdin`: data to feed on standard input
- `<name>.stdout`: data expected on standard output
- `<name>.stderr`: data expected on standard error from the test
  program (for compile-only tests, from GHC).

### Example

Test `T20757` is a regression test for issue [`#20757`][#20757].
The test program source code lives in
`testsuite/tests/ghc-api/T20757.hs`:

```haskell
module Main where

import GHC.SysTools.BaseDir

main :: IO ()
main = findToolDir False "/" >>= print
```

The test driver file `testsuite/tests/ghc-api/all.T` declares the
test (alongside several others):

```python
test('T20757',
     [unless(opsys('mingw32'), skip), exit_code(1)],
     compile_and_run,
     ['-package ghc'])
```

This declaration:

- tells the test driver to both compile and run the test program
- skips the test on operating systems other than Windows
- sets the expected exit status to `1`
- adds `-package ghc` to the *compiler* CLI options


[#20757]: https://gitlab.haskell.org/ghc/ghc/-/issues/20757

Additionally, `testsuite/tests/ghc-api/T20757.stderr` exists.  The
test driver shall assert that whatever the *test program* writes to
standard error matches the contents of that file.


## Running tests

I use [Hadrian] to build and test GHC:

```shell
% ./hadrian/build -j test
```

The `-j[N]` option sets the number of jobs that can be run in
parallel.  If the integer argument is not specified, it defaults to
the number of CPU cores.

There are around 9000 tests in the GHC test suite.  It takes a while
to run them all (~15 minutes with 12 parallel jobs on my 12-core
workstation).  If you're hacking on GHC you might want to limit the
driver to one or just a few tests.  Use the `--only` option to do
this:

```shell
% ./hadrian/build -j test --only=T20757
...
SUMMARY for test run started at Wed May 18 22:58:26 2022 
0:00:00.162930 spent to go through
       1 total tests, which gave rise to
       9 test cases, of which
       9 were skipped
       0 had missing libraries

       0 expected passes
       0 expected failures

       0 caused framework failures
       0 caused framework warnings
       0 unexpected passes
       0 unexpected failures
       0 unexpected stat failures
       0 fragile tests

Build completed in 1.30s
```

The driver ran zero tests.  Well, I am using FreeBSD; `T20757` skips
on all platforms except Windows.  Let's add one more test.  You can
specify multiple tests via `--only`, **separated by spaces**:

```shell
% ./hadrian/build -j test --only="T20757 executablePath"
...
SUMMARY for test run started at Wed May 18 23:02:33 2022
0:00:00.513652 spent to go through
       2 total tests, which gave rise to
      18 test cases, of which
      17 were skipped
       0 had missing libraries

       1 expected passes
       0 expected failures

       0 caused framework failures
       0 caused framework warnings
       0 unexpected passes
       0 unexpected failures
       0 unexpected stat failures
       0 fragile tests

Build completed in 11.82s
```

That's more like it.

Each test seems to inflate to 9 *test cases*.  I think these
correspond to the different [*compiler ways*][ways], and the test is
only run for the configured way(s).

Use `--test-verbose=[1,2,3,4,5]` to see more verbose output.  The
commands and output for compiling and running the test program
appear at level `3` and above.

See the GHC wiki for further [details about running tests][running].

[ways]: https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/rts/compiler-ways
[running]: https://gitlab.haskell.org/ghc/ghc/-/wikis/building/running-tests/running

## Testing `executablePath`

In my [previous post][] I described
[`System.Environment.executablePath`][executablePath], an improved
way to query the path to the executable file of the calling process.

```haskell
executablePath :: Maybe (IO (Maybe FilePath))
```

The `IO` query is not defined on all platforms.  Where it is
defined, its behaviour differs by platform.  So it is an interesting
feature to test.

[previous post]: 2022-05-10-improved-executable-path-queries.html
[executablePath]: https://downloads.haskell.org/ghc/9.4.1-alpha1/docs/html/libraries/base/System-Environment.html#v:executablePath

One could write multiple small test programs, one for each operating
system.  Then tell the test driver to run the test for the current
system, and skip the others.  Alternatively, define a single test
program, but tell it what platform it's running on.  That is what I
did.

The test driver declaration adds the operating system to the test
program's arguments:

```python
test('executablePath',
     extra_run_opts(config.os),
     compile_and_run,
     [''])
```

The test program itself then implements some OS-aware checks of the
behaviour of `executablePath`.  First come lists of which systems
have what behaviour:

```haskell
canQuery = ["mingw32","freebsd","linux","darwin","netbsd"]
canDelete = ["freebsd","linux","darwin","netbsd"]
canQueryAfterDelete = ["netbsd"]
```

In `main`, grab the OS from the program arguments:

```haskell
main :: IO ()
main = do
  [os] <- getArgs
```

Next grab the query function.  If the query is *expectedly*
undefined, stop here (`exitSuccess`).  If the query is *unexpected*
undefined, or *unexpectedly defined*, fail the test.  Otherwise
return the query.

```haskell
  query <- case (os `elem` canQuery, executablePath) of
    (False, Nothing) -> exitSuccess
    (False, Just _) -> die "query unexpectedly defined"
    (True, Nothing) -> die "query unexpected not defined"
    (True, Just k) -> pure k
```

Now run the query.  If it returns `Nothing`, fail the test
(it should return a result).

```haskell
  before <- query >>= \r -> case r of
    Nothing   -> die "query unexpectedly returned Nothing"
    Just path -> pure path
```

We need to compare the result (`before`) to the expected value.
That is, the file `executablePath` in the current directory:

```haskell
  cwd <- getCurrentDirectory
  let expected = cwd </> "executablePath"
```

Also drop the file extension (if any) from the result of the query.
This is needed because GHC names the executables it generates with a
file extension on some platforms (e.g. `.exe` on Windows).

```haskell
  let actual = dropExtension before
```

Now compare `expected` and `actual`.  Use `equalFilePath` because
the query may return a non-normalised path on some systems (I have
observed this on NetBSD):

```haskell
  unless (equalFilePath actual expected) $
    die "query result did not match expected"
```

Now, what happens if we *delete the executable* while the process
runs?  First of all, some operating systems don't even allow
that.  We grant those systems an honourable discharge.  The
remaining systems delete the file.

```haskell
  unless (os `elem` canDelete)
    exitSuccess
  removeFile before
```

Finally we run the query again.  Once again, the expected behaviour
differs by platform.  On Mac OS X and FreeBSD, we expect `Nothing`.
But NetBSD successfully returns the original value.

```haskell
  after <- query
  case after of
    Nothing
      | os `elem` canQueryAfterDelete
      -> die "query unexpected failed after delete"
      | otherwise
      -> pure ()
    Just _
      | os `elem` canQueryAfterDelete
      -> pure ()
      | otherwise
      -> die $ "query unexpected succeeded after delete"
```

Phew, quite a lot of code to test one little feature.  There is no
standard system interface for querying the executable path.  So it
is no surprise to see such diverse behaviour across different
platforms—including no query mechanism at all (looking at you,
OpenBSD).

## Conclusion

In this article I gave an introduction to writing tests for the GHC
test suite, with some examples.  The GHC wiki [contains more
comprehensive documentation][adding].  [Performance
tests][performance-tests] are a more complex aspect of the GHC test
suite which I didn't discuss in detail.

[adding]: https://gitlab.haskell.org/ghc/ghc/-/wikis/building/running-tests/adding
