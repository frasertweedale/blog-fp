---
tags: beginner-friendly, ghc
---

# Type-level programming for safer resource management

I find type-level naturals useful for enforcing proper usage of APIs
or protocols that involve transactions, locking, memory
(de)allocation, and similar concerns.  In this post I demonstrate
the main idea and discuss some of the shortcomings when using this
technique with Haskell.

## Overview of the technique

Consider the following API.

```haskell
-- provides type-level numeric literals
{-# LANGUAGE DataKinds #-}  -- 

-- provides type-level numeric operations
import GHC.TypeLits

data DbHandle n

openDatabase      :: IO (DbHandle 0)
startTransaction  :: DbHandle n -> IO (DbHandle (n + 1))
commitTransaction :: DbHandle n -> IO (DbHandle (n - 1))
closeDatabase     :: DbHandle 0 -> IO ()
```

The phantom type parameter `n` in `DbHandle n` tracks the
level of nested transactions.  The database is initially opened at
level `0` and can only be closed when the level is `0`.  The API
enforces that transactions must be committed.  (Error handling and
rollback is left as an exercise for the reader.)

Here's a trivial example of using this API:

```haskell
main :: IO ()
main = do
  db <- startTransaction =<< openDatabase
  -- do stuff
  commitTransaction db >>= closeDatabase
```

All happy.  But if we fail to commit the transaction…

```haskell
main = do
  db <- startTransaction =<< openDatabase
  -- do stuff
  closeDatabase db
```

…then we get a type error:

```
ExampleX.hs:21:9: error: [GHC-83865]
    • Couldn't match type ‘1’ with ‘0’
      Expected: DbHandle 0 -> IO (DbHandle 0)
        Actual: DbHandle 0 -> IO (DbHandle (0 + 1))
    • In the first argument of ‘(=<<)’, namely ‘startTransaction’
      In a stmt of a 'do' block: db <- startTransaction =<< openDatabase
      In the expression:
        do db <- startTransaction =<< openDatabase
           closeDatabase db
   |
21 |   db <- startTransaction =<< openDatabase
   |         .hs:21:9: error: [GHC-83865]
    • Couldn't match type ‘1’ with ‘0’
      Expected: DbHandle 0 -> IO (DbHandle 0)
        Actual: DbHandle 0 -> IO (DbHandle (0 + 1))
    • In the first argument of ‘(=<<)’, namely ‘startTransaction’
      In a stmt of a 'do' block: db <- startTransaction =<< openDatabase
      In the expression:
        do db <- startTransaction =<< openDatabase
           closeDatabase db
   |
21 |   db <- startTransaction =<< openDatabase
   |
```

Very well.


## Unnatural inhabitants

Because GHC's type-level naturals are not a true inductive type,
subtraction is allowed even when the result with be less than zero.
In this case, the value just appears as an un-normalised `(0 - 1)`.
We can see this in the type error when we attempt to compile the
following program:

```haskell
main = do
  db <- openDatabase  -- we didn't start a transaction
  -- do stuff
  commitTransaction db >>= closeDatabase
```

```
X.hs:23:3: error: [GHC-83865]
    • Couldn't match type ‘0 - 1’ with ‘0’
      Expected: IO (DbHandle 0)
        Actual: IO (DbHandle (0 - 1))
```

Unfortunately, the compiler could accept a program that commits a
transaction when no transaction is in progress.  The following
program compiles without error:

```haskell
main = do
  db <- openDatabase
  -- do stuff
  commitTransaction db  -- we didn't start a transaction
  pure ()               -- we didn't close the database
```


## Constraints to the rescue

We can add a constraint to `commitTransaction` to ensure that it can
only be applied to handles that are in a (possibly nested)
transaction:

```haskell
commitTransaction
  :: (1 <= n)
  => DbHandle n -> IO (DbHandle (n - 1))
```

Compiling the previous program now results in an error:

```
X.hs:25:3: error: [GHC-64725]
    • Cannot satisfy: 1 <= 0
    • In a stmt of a 'do' block: commitTransaction db
      In the expression:
        do db <- openDatabase
           commitTransaction db
           pure ()

```

And the original (correct) program continues still compiles.  But
now it emits a *redundant constraint* warning:

```
X.hs:15:6: warning: [GHC-30606] [-Wredundant-constraints]
    Redundant constraint: 1 <= n
    In the type signature for:
         commitTransaction :: forall (n :: Natural).
                              (1 <= n) =>
                              DbHandle n -> IO (DbHandle (n - 1))
   |
15 |   :: (1 <= n)
```

This is a bit annoying.  I don't know how to dispel this error
except to disable `-Wredundant-constraints`, and I don't want to do
that.  But in the end, these few spurious warnings are a small price
to pay for a safer API that prevents incorrect use at compile time.


## Ergonomics

User code typically use locks, transactions or allocation in a
*symmetric* way.  In other words, they initialise a context, perform
some actions, then tidy up.  It makes sense to wrap this pattern up
in a function that handles the bookkeeping:

```haskell
withTransaction
  :: DbHandle (n :: Natural)
  -> (forall m. DbHandle (m :: Natural) -> IO a)
  -> IO a
withTransaction db k = do
  db' <- startTransaction db
  r <- k db
  commitTransaction db'
  pure r
```

The `forall m.` (universal quantification) prevents improper use of
the database handle (e.g. premature commit) by requiring the action
function to work with database handles with any transaction level
(including `0`).

Unfortunately this code does not compile:

```
X.hs:28:3: error: [GHC-64725]
    • Cannot satisfy: 1 <= n + 1
    • In a stmt of a 'do' block: commitTransaction db'
      In the expression:
        do db' <- startTransaction db
           r <- k db
           commitTransaction db'
           pure r
```

The proposition to be satisfied is obviously true.  But again the
lack of inductive reasoning in GHC's type-level naturals bites us.
Sprinkling this constraint into the type signature solves the
problem (and also lets us avoid the explicit kind signatures):

```haskell
withTransaction
  :: (1 <= n + 1)
  => DbHandle n
  -> (forall m. (1 <= m + 1) => DbHandle m -> IO a)
  -> IO a
```

Now we can write a program that uses transactions like so:

```haskell
main = do
  db <- openDatabase
  withTransaction db (const $ pure ())
  closeDatabase db
```

Nested transactions also work:

```haskell
main = do
  db <- openDatabase
  withTransaction db
    ( \db' -> withTransaction db' (const $ pure ()) )
  closeDatabase db
```

But operations that rely on knowing the transaction level—like
committing—are excluded.  This program:

```haskell
main = do
  db <- openDatabase
  withTransaction db (\db' -> commitTransaction db')
  closeDatabase db
```

…results in a type error:

```
X.hs:38:31: error: [GHC-64725]
    • Cannot satisfy: 1 <= m
    • In the expression: commitTransaction db'
      In the second argument of ‘withTransaction’, namely
        ‘(\ db' -> commitTransaction db')’
      In a stmt of a 'do' block:
        withTransaction db (\ db' -> commitTransaction db')
   |
38 |   withTransaction db (\db' -> commitTransaction db')
   |                               ^^^^^^^^^^^^^^^^^
```

Which is exactly what we want.


## Conclusion

I have demonstrated a technique that enables safer APIs for
resource-related operations such as locking/unlocking, transactions,
and memory management.  Some type-level boilerplate is needed to
work around shortcomings in Haskell's type-level naturals.

We also discussed an idea to make the the API more ergonomic by
providing a "wrapper" function that handles the initialisation and
cleanup.  You would need additional functionality to safely handle
special operations that *should* be callable by user code, such as
rollbacks.  One idea is to use a sum type that allows
user code to request special operations.  For example:

```haskell
data TransactionResult a = Rollback | Commit a

withTransaction
  :: (1 <= n + 1)
  => DbHandle n
  -> (forall m. (1 <= m + 1) => DbHandle m -> TransactionResult a)
  -> IO (TransactionResult a)
withTransaction db k = do
  db' <- startTransaction db
  r <- k db
  case r of
    Rollback -> rollback db' $> r
    Commit _ -> pure r
```

This is just a sketch of the idea (I haven't tried it myself yet).
I encourage interested readers to explore further and share their
results.
