---
tags: ffi, gotcha
---

# Haskell FFI call safety and garbage collection

The Haskell *Foreign Function Interface (FFI)* lets you interface
with code written in other languages, including C.  Some kinds of
foreign calls—such as those that could call back into Haskell
code—require the GHC *runtime system (RTS)* to do some bookkeeping.
This bookkeeping has a performance cost, so there is a mechanism to
out of it for foreign calls that can't call back into Haskell.  This
mechanism is called the *safety level*.  There are two levels:

- **`safe`**: do the bookkeeping; callbacks are safe
- **`unsafe`**: skip the bookkeeping; callbacks have undefined
  behaviour

But beware!  Besides callback safety, there are other situations
that require a `safe` foreign call.  And some that may require an
`unsafe` call (not just for performance).  [In this post I explain
the garbage collection behaviour of `safe` and `unsafe` foreign
calls, and describe how the wrong choice led to a nasty deadlock bug
in *hs-notmuch*.]{.abstract}


## Foreign imports

[Chapter 8 of the Haskell 2010 Language Report][ch8] specifies the
foreign function interface syntax and semantics.  A `foreign import`
declaration creates a Haskell binding to a foreign function or
value:

```haskell
foreign import ccall unsafe "notmuch.h notmuch_database_open"
  notmuch_database_open
    :: CString
    -> CInt
    -> Ptr (Ptr DatabaseHandle)
    -> IO CInt
```

You can see that the `foreign import` declaration contains:

- the safety declaration (`unsafe`)
- a reference to the C header and symbol to be imported
- a name for the function on the Haskell side
- a type annotation, which corresponds to the C type signature

If you need a `safe` foreign call, write `safe` or just omit the
safety declaration (`safe` is the default).

[ch8]: https://www.haskell.org/onlinereport/haskell2010/haskellch8.html

::: note

`notmuch_database_open` is a C *double-pointer style constructor*.
The arguments are the filesystem path (`CString`), a mode enum
(`CInt`) and a location to write the pointer to the database handle
upon success (`Ptr (Ptr DatabaseHandle)`).  The return value is `0`
on success or a nonzero error code (`CInt`).

:::


## Finalizers

Haskell is a garbage collected language.  It is possible to use the
garbage collector to clean up objects that were allocated in foreign
calls, when they are no longer referenced.  The clean up functions
are called *finalizers*.  Often, finalizers are themselves imported
from the foreign library:

```haskell
foreign import ccall "notmuch.h &notmuch_database_destroy"
  notmuch_database_destroy :: FinalizerPtr DatabaseHandle
```

The ampersand (`&`) denotes that we are importing a *function
pointer* rather than the function itself.

`FinalizerPtr` is a type synonym defined in the
[`Foreign.ForeignPtr`][doc-ForeignPtr] module:

[doc-ForeignPtr]: https://hackage.haskell.org/package/base-4.16.2.0/docs/Foreign-ForeignPtr.html#t:FinalizerPtr

```haskell
type FinalizerPtr a = FunPtr (Ptr a -> IO ())
```

This arises from the usual definition of a destructor or `free`
function.  That is, a void function whose single argument is the
pointer to the object to be destroyed, or memory to be freed.

Programs need to associate finalizers with the objects they are to
clean up.  The function to do this is
[`newForeignPtr`][doc-newForeignPtr]:

```haskell
newForeignPtr
  :: FinalizerPtr a -> Ptr a -> IO (ForeignPtr a)
```

::: note

A `ForeignPtr a` can have multiple (or zero) finalizers.  Use cases
for multiple finalizers are uncommon.

:::

[doc-newForeignPtr]: https://hackage.haskell.org/package/base-4.16.2.0/docs/Foreign-ForeignPtr.html#v:newForeignPtr


## FFI safety and garbage collection

Consider the wording of the Haskell 2010 FFI chapter:

> A `safe` call … guarantees to leave the Haskell system in a state
> that allows callbacks from the external code. In contrast, an
> `unsafe` call, while carrying less overhead, must not trigger a
> callback into the Haskell system. If it does, the system behaviour
> is undefined. … Note that a callback into the Haskell system
> implies that a garbage collection might be triggered after an
> external entity was called, but before this call returns.

This says that garbage collection can occur during a `safe` call.
But it *does not say* whether GC is allowed, or not, during an
`unsafe` call.  It is up to implementations to decide what to do.

GHC's behaviour here has changed over time.  Since version 8.4, GHC
*guarantees* that **garbage collection will never occur during an
`unsafe` FFI call.** This guarantee allows `unsafe` FFI calls to
work with heap-allocated data, which enables some performance
optimisations.

::: note

The [GHC users guide][guide-ffi] has a more thorough treatment of
this topic.  It also mentions important details about threading
and the FFI, among other things.

:::

[guide-ffi]: https://downloads.haskell.org/ghc/9.4.1/docs/users_guide/exts/ffi.html#guaranteed-call-safety


## Crouching GC, hidden deadlock

We have discussed the FFI, finalizers, foreign call (un)safety and
garbage collection.  What's it all coming to?

The earlier foreign import examples are from
[*hs-notmuch*][hs-notmuch], my Haskell binding to the
[*notmuch*][notmuch] mail indexer.  Note the following:

- `notmuch_database_open` is an `unsafe` foreign call (because it
  doesn't call back into Haskell and I don't want the bookkeeping
  overhead).

- `notmuch_database_destroy` is a finalizer that closes the database
  and frees resources.  The garbage collector schedules the
  finalizer when the database handle is no longer in use.

- Wrapper code in *hs-notmuch* uses `newForeignPtr` to associate the
  the `notmuch_database_destroy` finalizer with the pointers created
  by `notmuch_database_open`.

- The finalizer (called after GC) is the *only way* to close a
  database handle.  The *hs-notmuch* API does not offer an explicit
  close function.

[hs-notmuch]: https://hackage.haskell.org/package/notmuch
[notmuch]: https://notmuchmail.org/

An application could attempt to open a database multiple times.
This might be intentional.  Or it could occur when there is an
unreferenced database handle whose finalizer has not yet been
executed.

*libnotmuch* uses locks to prevent multiple read-write sessions to a
single database.  `notmuch_database_open` blocks if the lock is
already held.  In the case of *accidental* multiple open this isn't
a problem because GC will eventually occur, finalizers will run and
the lock will be released.

**Except it won't, because GHC prevents garbage collection during
`unsafe` foreign calls.**  As a result, the program deadlocks.
Non-deterministically.

This bug went unnoticed for a long time.  It was [eventually
detected][] by [*purebred*][purebred]'s automated user acceptance
tests, which perform many user actions very quickly.  (*purebred* is
a mail program that uses *hs-notmuch*).  Whether deadlock is likely
to occur depends very much on the application and/or user behaviour.

[eventually detected]: https://github.com/purebred-mua/purebred/issues/468
[purebred]: https://github.com/purebred-mua/purebred

Fortunately, the fix was simple: make `notmuch_database_open` a
`safe` foreign call.  Opening the database would typically be an
infrequent operation so the bookkeeping overhead is tolerable.


## Conclusion

This post discussed the FFI, finalizers, and GHC's garbage
collection behaviour (or lack thereof) during `safe` and `unsafe`
foreign calls.  I used a deadlock bug in a foreign binding library
as a case study of this behaviour.

The folk wisdom regarding `safe` versus `unsafe` foreign calls
mainly deals with callbacks and performance overheads.  I have
rarely seen the garbage collection mentioned.  This is unfortunate
because the GC behaviour is critical to program safety and
correctness (as the case study proves).  Resources (wiki pages, blog
posts, etc) that discuss FFI call safety but fail to mention the GC
behaviour of `safe` versus `unsafe` should be updated.

With these things in mind, here are my recommendations for Haskell
programmers working with the FFI:

- If a foreign function could call back into Haskell code, it must
  be `safe`.

- If a foreign call might block, it probably needs to be `safe`
  (unless you are certain about what you are doing).

- If you are unsure about whether a foreign call could block (or
  why), make it `safe`.

In fact, it's fine to make every foreign import `safe` unless:

- You need to guarantee that heap-allocated objects (e.g. unpinned
  `ByteArray#`) will not move during the foreign call, or

- The bookkeeping overhead is a real performance issue (e.g. C-style
  `_valid()`/`_get()`/`_next()` iterators, calls in tight loops).

Doing so might deliver you from debugging a non-deterministic
deadlock.
