---
tags: ffi
---

# Fixing `getExecutablePath` on FreeBSD

[`System.Environment.getExecutablePath`][getExecutablePath] (part of
the *base* library) returns the absolute pathname of the current
executable.  Except when it doesn't.  From my work on [Dyre][dyre]
(a **dy**namic **re**configuration system for Haskell programs) I
discovered that `getExecutablePath` did not work properly on
FreeBSD.  Fixing it involved FreeBSD's [`sysctl(3)`][sysctl]
interface and the Haskell [*Foreign Function Interface*][ffi]
*(FFI)*.

[dyre]: https://hackage.haskell.org/package/dyre
[getExecutablePath]: https://hackage.haskell.org/package/base-4.14.1.0/docs/System-Environment.html#v:getExecutablePath
[sysctl]: https://www.freebsd.org/cgi/man.cgi?query=sysctl&sektion=3&manpath=FreeBSD+12.2-RELEASE
[ffi]: https://downloads.haskell.org/ghc/8.10.1/docs/html/users_guide/ffi-chap.html

## How `getExecutablePath` is implemented

The *base* library is part of GHC.  `getExecutablePath` is defined
in [`libraries/base/System/Environment/ExecutablePath.hsc`][hsc]
(link is to the GHC 8.6.5 version).  `.hsc` files are processed by
[`hsc2hs`][hsc2hs], which provides helper directives for working
with the FFI (it is also distributed with GHC).

[hsc]: https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-8.6.5-release/libraries/base/System/Environment/ExecutablePath.hsc
[hsc2hs]: https://github.com/haskell/hsc2hs

`ExecutablePath.hsc` uses C Preprocessor (CPP) directives to select
different implementations of `getExecutablePath` for different
operating systems.  In GHC 8.6 and earlier there were
platform-specific implementations for Linux, Mac OS X and Windows
(all the [GHC Tier 1 platforms][tier1]).  And there is a fallback
implementation when no platform-specific implementation is available
(we shall see that it has some problems).  All of the
implementations use the FFI to some extent.

[tier1]: https://gitlab.haskell.org/ghc/ghc/-/wikis/platforms#tier-1-platforms

### The Linux implementation

Linux systems almost always mount [`procfs(5)`][linux-procfs], a
filesystem of information about running processes.  Information
about a particular process is published in files under
`/proc/{pid}/`.  A process can also find information about itself
under `/proc/self/`.

[linux-procfs]: https://en.wikipedia.org/wiki/Procfs#Linux

A process can find out its own executable by reading
`/proc/self/exe`, which is a symbolic link.  And this is exactly
what the implementation of `getExecutablePath` for Linux does:

```haskell
getExecutablePath = readSymbolicLink $ "/proc/self/exe"

readSymbolicLink :: FilePath -> IO FilePath
readSymbolicLink file =
    -- uses readlink(3) via FFI
```


### The fallback implementation (has problems)

When the *base* library does not have an operating system-specific
implementation for `getExecutablePath` it falls back to `argv[0]`:
the first value in the program's argument array.  However, this is
not a reliable way to determine the actual executable that supplied
the program text.  Many programs and environments manipulate the
contents of `argv[0]`.  The `execv*(3)` family of calls, which
replaces the current process with a new process image, allow the
caller to specify an arbitrary `argv`.

As an example of `argv[0]` manipulation, consider
[`System.Posix.Process.executeFile`][executeFile] from the
[*unix*][unix] package.  `executeFile` calls one of the `execv*(3)`
subroutines (exactly which one depends on its own argument).  But,
as stated in the documentation:

> The basename (leading directory names suppressed) of the command
> is passed to execv\* as `arg[0]`.

So if you use `executeFile`, `argv[0]` of the resulting process does
not contain a full pathname.  And you may not be able to resolve the
*basename* to an executable via the `PATH` environment variable.
Worse still, you may successfully resolve it *to the wrong
executable!*

In fact, this was exactly the problem I experienced using *Dyre* on
FreeBSD.  *Dyre* automatically builds and caches an executable when
it detects that configuration has changed, and `exec(3)`s the cached
executable.  On FreeBSD, the combination of `executeFile` `argv[0]`
behaviour and use of the fallback `getExecutablePath` implementation
caused *Dyre* to never recognise that it was running the cached
executable.  As a result, it entered an infinite loop of
`execv*(3)`.

[executeFile]: https://hackage.haskell.org/package/unix-2.7.2.2/docs/System-Posix-Process.html#v:executeFile
[unix]: https://hackage.haskell.org/package/unix


## Querying the current executable on FreeBSD

It was clear that GHC needed a FreeBSD-specific implementation of
`getExecutablePath`.  So, what is the "proper" way to query the
current process' executable on FreeBSD?

[FreeBSD does implement a `procfs`][freebsd-procfs].  Some of the
file names differ from Linux's `procfs`.  For example, the "current
executable" symlink path is `/proc/curproc/file` instead of
`/proc/self/exe`.  It would be a small change to parameterise the
Linux implementation so that it can be used for FreeBSD.  But unlike
typical Linux systems, FreeBSD's `procfs` is often unused and not
even mounted (especially on servers).  Therefore the `procfs`
approach is not suitable for FreeBSD.

[freebsd-procfs]: https://www.freebsd.org/cgi/man.cgi?query=procfs&manpath=FreeBSD+12.2-RELEASE

A bit of research revealed that the [`sysctl(3)`][sysctl] interface
is the proper and ubiquitous way to query the executable path on
FreeBSD.  `sysctl(3)` exposes a *management information base (MIB)*,
a tree structure where nodes are identified by a sequence of
integers (*object identifier* or *OID*).  For example, the kernel
"maximum processes" setting has the OID `[1, 6]`.  Some (but not
all) OIDs have a string representation so that values can be queried
or changed by the userland `sysctl(8)` utility:

```
% sysctl kern.maxproc 
kern.maxproc: 3828
```

To assist programmers reading and writing programs that use
`sysctl(3)`, important OID components are defined in
`<sys/sysctl.h>`:

```c
/*
 * Top-level identifiers
 */
#define CTL_UNSPEC  0  /* unused */
#define CTL_KERN    1  /* "high kernel": proc, limits */
...

/*
 * CTL_KERN identifiers
 */
...
#define KERN_MAXPROC  6  /* int: max processes */
...
```

The OID containing a process' executable pathname is `[1, 14, 12,
$PID]` where `PID` is the process ID, or `-1` to refer to the
current process.  The C `#define`s for the first three components
are `CTL_KERN`, `KERN_PROC`, and `KERN_PROC_PATHNAME`.

Now that we know the OID, consider the signature of `sysctl(3)`:

```c
int sysctl(
    const int *name,
    u_int namelen,
    void *oldp,
    size_t *oldlenp,
    const void *newp,
    size_t newlen);
```

- `name` is the OID; an array of `int`.

- `namelen` is the number of components in `name`

- `oldp` is a pointer to a buffer to hold the current/old value.  It
  can be `NULL`.  It is a `void *` because sysctl nodes have a
  variety of types.

- `oldlenp` is an in/out argument that gives the size of `outp`
  before the call, and holds the actual size of the datum after the
  call (including when `oldp` is `NULL`).

- `newp` and `newlen` supply a new value.  These are not needed for
  my use case and I will set them to `NULL` and `0` respectively.

Therefore the procedure to query the current executable is:

1. Invoke `sysctl` with `oldp = NULL`.
2. Read `oldlenp` to determine size of datum.
3. Allocate big enough buffer.
4. Call `sysctl` again, with newly allocated buffer as `oldp`.
5. Process the data in `oldp`.
6. Free the buffer.

This is what the FreeBSD-specific implementation of
`getExecutablePath` must do.


## The FreeBSD implementation

I will now walk through the different parts of the FreeBSD-specific
implementation of `getExecutablePath`.

### C Preprocessor guards

System-specific implementations are guarded by CPP conditionals:

```c
#if defined(darwin_HOST_OS)

... MacOS implementation

#elif defined(linux_HOST_OS)

... Linux implementation

#elif defined(mingw32_HOST_OS)

... Windows implementation

#else

... fallback implementation

#endif
```

I added the appropriate guard to encapsulate the FreeBSD-specific
implementation: 

```c
#elif defined(freebsd_HOST_OS)

... FreeBSD implementation
```

Similarly, in the `import` area CPP conditionals wrap the
FreeBSD-specific Haskell imports and C header includes:

```haskell
#elif defined(freebsd_HOST_OS)
import Foreign.C
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import System.Posix.Internals
#include <sys/types.h>
#include <sys/sysctl.h>
```

### Foreign import

A `foreign import` declaration makes `sysctl(3)` available to
Haskell code as `c_sysctl`:

```haskell
foreign import ccall unsafe "sysctl"
  c_sysctl
    :: Ptr CInt   -- MIB
    -> CUInt      -- MIB size
    -> Ptr CChar  -- old / current value buffer
    -> Ptr CSize  -- old / current value buffer size
    -> Ptr CChar  -- new value
    -> CSize      -- new value size
    -> IO CInt    -- result
```

Observe the correspondence to the C function signature.  Because
this function is not referentially transparent (different calls with
the same arguments could have different results) the result type is
`IO CInt`.


### Defining the MIB

The `hsc2hs` `(#const ...)` directive assists in the definition of
the "current process executable" MIB:

```haskell
mib :: [CInt]
mib =
  [ (#const CTL_KERN)
  , (#const KERN_PROC)
  , (#const KERN_PROC_PATHNAME)
  , -1   -- current process
  ]
```

### Defining `getExecutablePath`

The `getExecutablePath` implementation closely follows the abstract
procedure outlined above.  So here it is, broken up with commentary
([full diff][]):

[full diff]: https://gitlab.haskell.org/ghc/ghc/-/merge_requests/1276/diffs

```haskell
getExecutablePath =
  withArrayLen mib $ \n mibPtr -> do
```

[`withArrayLen`][withArrayLen] converts a Haskell list to a
heap-allocated C array, passing it (as a pointer) *and* its length
to the given continuation function.  It will free the array after
the continuation returns.

[withArrayLen]: https://hackage.haskell.org/package/base-4.14.1.0/docs/Foreign-Marshal-Array.html#v:withArrayLen


```
    let mibLen = fromIntegral n
    alloca $ \bufSizePtr -> do
```

Convert `mibLen` from `Int` to `CInt`.  [`alloca`][alloca] allocates
memory to hold a `CSize` (`size_t`) and passes a pointer to that
memory to the given continuation (and frees it when the continuation
returns).

[alloca]: https://hackage.haskell.org/package/base-4.14.1.0/docs/Foreign-Marshal-Alloc.html#v:alloca

```haskell
      status <- c_sysctl mibPtr mibLen
                         nullPtr bufSizePtr nullPtr 0
```

Call `sysctl(3)` with `oldp = NULL`.

```haskell
      case status of
        0 -> do
          reqBufSize <- fromIntegral <$> peek bufSizePtr
          allocaBytes reqBufSize $ \buf -> do
```

If the initial call to `sysctl(3)` succeeded, `bufSizePtr` holds the
required buffer size.  [`peek`][peek] reads the value.  Then
[`allocaBytes`][allocaBytes] allocates that much space on the heap
and passes the pointer to the continuation (and frees it after).

[peek]: https://hackage.haskell.org/package/base-4.14.1.0/docs/Foreign-Storable.html#v:peek
[allocaBytes]: https://hackage.haskell.org/package/base-4.14.1.0/docs/Foreign-Marshal-Alloc.html#v:allocaBytes

```haskell
            newStatus <- c_sysctl mibPtr mibLen
                                  buf bufSizePtr nullPtr 0
            case newStatus of
              0 -> peekFilePath buf
              _ -> throwErrno "getExecutablePath"
        _ -> throwErrno "getExecutablePath"
```

Call `sysctl(3)` again, this time using the new buffer (`buf`) as
`oldp`.  If successful, [`peekFilePath`][peekFilePath] converts the
C string to a Haskell `FilePath`.  Although it is three
continuations deep, this `FilePath` will be the return value.  If
either call to `sysctl(3)` failed, [`throwErrno`][throwErrno] throws
an `IOError`.

[peekFilePath]: https://hackage.haskell.org/package/base-4.14.1.0/docs/System-Posix-Internals.html#v:peekFilePath
[throwErrno]: https://hackage.haskell.org/package/base-4.14.1.0/docs/Foreign-C-Error.html#v:throwErrno


## Conclusion

This was a small enhancement to GHC, but for me a critical one.  I
submitted a [merge request][].  It was quickly accepted and the fix
was released in GHC 8.8.1.

[merge request]: https://gitlab.haskell.org/ghc/ghc/-/merge_requests/1276

Although I didn't address it in my pull request, there is an error
in the [`getExecutablePath`][getExecutablePath] documentation.  The
`argv[0]` fallback implementation *does not* necessarily return an
absolute pathname, as claimed.  It would be better if
platform-specific and fallback behaviours were fully explained.

If you would like to see fixes, enhancements or documentation
improvements (*hint, hint*) in GHC, do not be afraid!  GHC is a huge
project.  But after you work out what bits go where, it's not
necessarily hard to make changes.  It helps a lot that it is written
in Haskell!  You can submit a pull request via the [Haskell GitLab
instance](https://gitlab.haskell.org/ghc/ghc) (you can sign in with
GitHub credentials).  Apart from this FreeBSD-related fix I have
submitted a couple other changes to GHC.  I have found the GHC
developers are always friendly and willing to assist new and less
experienced contributors.

By the way, I do not mean to trivialise GHC development.  Some parts
of it are truly daunting.  But many parts are not.  I believe that
no Haskell developer—not even beginners—should be daunted by the
*idea* of contributing to GHC, or believe that they cannot do it.

In terms of the [Haskell FFI][ffi], this change captured a few
interesting concepts, including:

* allocation memory of arbitrary size ([`alloca`][alloca])
* converting Haskell lists to C arrays ([`withArrayLen`][withArrayLen])
* reading data from pointers ([`peek`][peek])
* referencing constants via the `hsc2hs` `(#const ...)` directive

The *"allocate, call continuation and cleanup"* pattern is prevalent
in the Haskell `Foreign.*` modules.  Even this small example goes
three layers deep.  More complex FFI use cases can go much deeper.

Important FFI topics that this case study did not engage include
marshalling structs (something `hsc2hs` helps), `ForeignPtr`s and
finalizers, and callbacks.  So although I hope this case study may
be instructive in *some* aspects of Haskell FFI usage, it is far
from comprehensive.
