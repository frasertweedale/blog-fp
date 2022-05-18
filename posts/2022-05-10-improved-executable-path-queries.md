---
tags: ghc
---

# Better executable path queries in GHC 9.4

I [previously wrote about][previously]
[`System.Environment.getExecutablePath`][getExecutablePath] and how
I fixed it on FreeBSD.  Unfortunately, this function still has some
problems.  In this post I explain the problems and introduce
[`executablePath`][executablePath], the solution arriving in
*base-4.17.0.0* (GHC 9.4.1).

[previously]: 2021-01-01-fixing-getExecutablePath-FreeBSD.html
[getExecutablePath]: https://downloads.haskell.org/ghc/9.4.1-alpha1/docs/html/libraries/base/System-Environment.html#v:getExecutablePath
[executablePath]: https://downloads.haskell.org/ghc/9.4.1-alpha1/docs/html/libraries/base/System-Environment.html#v:executablePath


## Problems with `getExecutablePath`

`getExecutablePath :: IO FilePath` is a way for a Haskell program to
query the path to its own executable.  It has several significant
problems:

- **Not all operating systems provide a reliable mechanism to query
  the executable path.**  Where an OS-specific implementation does
  not exist, `getExecutablePath` falls back to providing the value
  of `argv[0]` ([#12377][]).  The invoking process chooses the
  value; it does not necessarily represent the path to the
  executable.  It might represent or resolve to a different
  executable.  `argv` could even be an empty array, in which case
  `getExecutablePath` throws an exception!

- **Divergent behaviour when executable has been deleted.**  When we
  say "executable" we mean "*file which contains **program** text,
  which the OS can load and execute (becoming a **process**)*".
  That file could be deleted while the process is running.  In this
  case, the behaviour of `getExecutablePath` differs by platform.
  On FreeBSD it throws an exception.  On Linux it returns the
  original `FilePath` suffixed with `" (deleted)"` ([#10957][]).
  These differences impede cross-platform development.

- **The documentation is wrong.**  Until I fixed it, the
  documentation for `getExecutablePath` stated, *"Returns the
  absolute pathname of the current executable."*  It didn't explain
  any of the discrepancies mentioned in the preceding points.
  Programmers can easily stumble into the unsafe behaviour (I did).

[#12377]: https://gitlab.haskell.org/ghc/ghc/-/issues/12377
[#10957]: https://gitlab.haskell.org/ghc/ghc/-/issues/10957


## Type of the solution

Types are an essential tool for modelling a problem and guiding the
development of a solution.  The problems with `getExecutablePath`
reveal that:

- Some OSes provide a mechanism to query the executable path, and
  some do not.  This is a static property of the platform; it does
  not change over the lifetime of a process.

- The query mechanism (if it exists) might be unable to return a
  result.  For example, when the executable file has been deleted.
  The result may vary during the lifetime of a process.

The `Maybe a` type models the existence or absence of a value:

```haskell
data Maybe a = Nothing | Just a
```

Accordingly, a suitable type to model this problem is:

```haskell
executablePath :: Maybe (IO (Maybe FilePath))
```

The outer `Maybe` models the presence or absence of a query
mechanism.  The query itself has the type `IO (Maybe FilePath)`.
The inner `Maybe` models that the query might be unable to return
a valid `FilePath`.

The type is also a kind of (machine-checked) documentation.  It
reveals things that the written documentation for
`getExecutablePath` ***should have said, but didn't***.

::: note

`FilePath` is defined as a type synonym for `String`, which is
itself a type synonym for `[Char]`:

```haskell
type FilePath = String
type String   = [Char]
```

It can be argued on multiple grounds that this is not an appropriate
type for representing file paths:

- Performance: `[Char]` is a linked list of individual characters.
  Packed strings have better performance.

- Correctness: `FilePath` admits any string value, not just valid
  paths.  See above for a real world example: paths suffixed with
  `"(deleted)"` on Linux.

I did not go further down this rabbit hole for the change discussed
in this post.  `FilePath` pervades *base* and other "standard"
libraries.  Furthermore, GHC targets a variety of operating systems;
accurately modeling valid file paths on diverse platforms drives up
complexity.  If you have specific needs not met by `FilePath`, check
out the [many path libraries][] which offer different approaches to
representing and working with paths.

[many path libraries]: https://hackage.haskell.org/packages/search?terms=filepath

:::

## Implementation of `executablePath`

In this section I'll briefly review the implementation.  GHC
[merge request !4779][!4779] has the gory details, for those
interested.

[!4779]: https://gitlab.haskell.org/ghc/ghc/-/merge_requests/4779

I was able to implement `executablePath` without modifying any code
that uses the *foreign function interface (FFI)*.
`getExecutablePath` was unchanged.  `executablePath` implementations
wrap the former.  See [my earlier post][previously] for an example
of how `getExecutablePath` uses the FFI.

### Mac OS X, FreeBSD and NetBSD

The FreeBSD and NetBSD implementations of `getExecutablePath` are
nearly identical, but the implementation for Mac OS X is very
different.  Nevertheless, the observable behaviour is identical: the
system calls error with `ENOENT` when the executable has been
deleted, and succeed otherwise.  No other expected failure scenarios
are known (yet).

Therefore, the `executablePath` implementation for these platforms
boils down to catching the Haskell exception value corresponding to
`ENOENT` and turning it into `Nothing`.  Unexpected exceptions are
re-thrown.

```haskell
executablePath =
  Just (fmap Just getExecutablePath `catch` f)
    where
    f e | isDoesNotExistError e = pure Nothing
        | otherwise             = throw e
```

### Linux

The Linux implementation of `getExecutablePath` reads the value of
`/proc/self/exe` (part of the [`procfs(5)`][debian-procfs]).  The
man page states:

> If the pathname has been unlinked, the symbolic link will contain
> the string '(deleted)' appended to the original pathname.

[debian-procfs]: https://manpages.debian.org/buster/manpages/procfs.5.en.html

`executablePath` checks for this condition and, if detected, returns
`Nothing`.  Note that we could have stripped the suffix and returned
`Just` the "original" path.  Returning `Nothing` makes it consistent
with the other platforms.

```haskell
executablePath = Just (fmap check getExecutablePath)
  where
  check s | "(deleted)" `isSuffixOf` s = Nothing
          | otherwise                  = Just s
```

::: note

What if the file is named `foo (deleted)`?  The behaviour is
ambiguous.  Checking the existence of the file is not safe either.
If the file was `foo`, a *different* file `foo (deleted)` could
exist beside it.  Better a false negative in an unlikely scenario,
than an **unsafe false positive**.

:::


### Windows

Windows prevents the deletion of an executable file during the
lifetime of any process created from it.  So `executablePath` simply
wraps the result of `getExecutablePath` with a `Just`.

```haskell
executablePath = Just (fmap Just getExecutablePath)
```

### Fallback implementation

The "fallback implementation" is for platforms that don't have a
reliable mechanism for querying the executable path (or no one
implemented it in GHC yet).  In this case, `executablePath` does not
even supply the query `IO` action.

```haskell
executablePath = Nothing
```

Programs that want to query the executable path have to deal with
the `Nothing` case.  That is: the possibility that there *is no
reliable way* to get it.  That's a good thing.


## Conclusion

This article explained the problems of `getExecutablePath` and
reviewed the solution coming in GHC 9.4, called `executablePath`.  I
encourage programs that use `getExecutablePath` to migrate when
feasible, especially if multi-platform support is important.

One topic I did not discuss is how I implemented tests for this
feature in the GHC test suite.  I will cover this in an upcoming
post.
