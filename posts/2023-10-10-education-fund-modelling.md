---
tags: finance, beginner-friendly
---

# Education fund modelling with Haskell

Like most people, I don't like big financial surprises or sudden,
substantial changes to cashflow.  Although we can't control all
circumstances, we can plan for projected future expenses, like your
kids' education.  [In this post I share a basic model built in
Haskell to help plan for education expenses (or other large, future,
time-bounded expenses).]{.abstract}

This **beginner-friendly post** demonstrates many simple Haskell
functions, especially for working with lists.  It also shows how to
build and execute stateful computations using `State` from *mtl*.  I
(mostly) avoid type signatures and just focus on defining the terms,
but there are plenty of links to API documentation.  At the end of
the post I suggest some enhancements to the model that would be good
**exercises for learners** (and might be fun even for more
experienced Haskell programmers).

## Scenario and simplifying assumptions

The general scenario I built the model for is to save for private
secondary **school fees for two children**.  They are 3 years apart
with the older child commencing in 4 years.  Costs of primary
(elementary) schooling are not considered in this scenario, although
the model would accommodate that.

Given the time span (> 10 years) we have to consider **inflation**.
The model uses a constant rate of inflation of 5% per annum, which
is less than the rate of inflation in Australia at time of writing,
but more than the our RBA's long term target of 2–3%.

We also model an annual **investment return** of 8%.  Short-term
volatility is inevitable but this is less than the *long-term
average* for the Australian stock market.

**Contributions** to the fund will be annual.  In real life, for
stable cashflow and to achieve [*dollar cost averaging*][DCA],
contributions could be made more regularly (e.g. each pay day).  The
model is therefore slightly pessimistic in this regard, but simpler
to implement.

[DCA]: https://en.wikipedia.org/wiki/Dollar_cost_averaging


## General description of the model

The inputs to the model are a **fee structure**, and an annual
contribution amount which is fixed (does not grow with inflation or
income).

The model projects the costs of education in future years, and works
backwards to determine how much money needs to be in the fund at the
start of each year.  The output of the model is a list of these
required balances, the first of which is the required **initial
starting balance** for the education fund.  If the starting balance
is too high, increase the yearly contribution and evaluate the model
again.  Continue until you find a balance between starting value and
contribution amount that works for you.

## Modelling the costs

A school's *current* fee structure, for the six years of secondary
education, is the ordered list of these numbers:

```haskell
feesBase =
  [ 12000, 12000, 12000     -- grade  7,  8,  9
  , 13500, 13500, 13500 ]   -- grade 10, 11, 12
```

Child 1 will be starting high school in 4 years; Child 2 in 7 years.
We consider the intervening years to have nil cost, which we
represent by using [`replicate`][haddock-replicate] to make lists of
`0` of the required lengths.  We append these sublists using
[`<>`][haddock-append].  We also extend the *shorter* list with
additional zeroes (*only* the shorter list, because
[`repeat`][haddock-repeat] makes an **infinite list**).

```haskell
feesChild1Base =
  replicate 4 0 <> feesBase <> repeat 0

feesChild2Base =
  replicate 7 0 <> feesBase
```

::: note

The [`<>`][haddock-append] function appends many types, where the
operation is associative and the result is always defined.  You can
also append lists with [`++`][haddock-list-append], which is
specific to the list type.

:::

We could add the yearly fees using [`zipWith`][haddock-zipWith],
whose arguments are a binary combining function and two lists:

```haskell
feesCombinedBase =
  zipWith (+) feesChild1Base feesChild2Base
```

However, many schools offer discounts when you have multiple
children enrolled.  In this scenario, the school gives a 10%
discount for the younger (cheaper) child, when both are enrolled.
We define the combining function using [`min`][haddock-min],
[`max`][haddock-max], addition and multiplication:

```haskell
-- define our custom combining function...
sumWithDiscount a b =
  max a b + min a b * 0.9

-- ... and update feesCombinedBase to use it
feesCombinedBase =
  zipWith sumWithDiscount feesChild1Base feesChild2Base
```

Let's evaluate `feesCombinedBase` and [`print`][haddock-print] the
values.  [`traverse_`][haddock-traverse] applies an action to each
element of a list (or other container), then discards the result.

```
λ> traverse_ print feesCombinedBase
0.0
0.0
0.0
0.0
12000.0
12000.0
12000.0
24300.0
24300.0
24300.0
13500.0
13500.0
13500.0
```

Now **inflation** must have its way with these numbers.  Use
[`iterate`][haddock-iterate] to generate the inflation factor for
successive years (*ad infinitum*) by iteratively apply our inflation
rate, starting at `1`.  To make the numbers presentable we'll also
[`round`][haddock-round] to 4 decimal places.

```haskell
round4    = (/ 10000) . fromIntegral . round . (* 10000)
inflation = fmap round4 (iterate (* 1.05) 1)
```

::: note

[`fmap`][haddock-fmap] applies a function (the first argument) to
every element of a container or producer (the second argument).

:::

Then we can apply the inflation to our uninflated costs by year.
One thing I did not yet mention is that the fees above are nearly a
year old and will be going up soon, so we'll use
[`drop`][haddock-drop] to "shift left" the inflation figures by one
year.  Once more we use `zipWith` for a very neat expression:

```haskell
feesCombinedInflated =
  zipWith (*) feesCombinedBase (drop 1 inflation)
```

Let's `print` the projected fees:

```
λ> traverse_ print feesCombinedInflated
0.0
0.0
0.0
0.0
15315.6
16081.2
16885.2
35903.25
37696.59
39582.27
23089.05
24244.65
25455.6
```

This looks right.  The highest costs are in the three "overlap"
years where both children are enrolled, and the impact of inflation
is evident.

[haddock-replicate]: https://hackage.haskell.org/package/base-4.19.0.0/docs/Prelude.html#v:replicate
[haddock-append]: https://hackage.haskell.org/package/base-4.19.0.0/docs/Prelude.html#v:-60--62-
[haddock-list-append]: https://hackage.haskell.org/package/base-4.19.0.0/docs/Prelude.html#v:-43--43-
[haddock-repeat]: https://hackage.haskell.org/package/base-4.19.0.0/docs/Prelude.html#v:repeat
[haddock-zipWith]: https://hackage.haskell.org/package/base-4.19.0.0/docs/Prelude.html#v:zipWith
[haddock-min]: https://hackage.haskell.org/package/base-4.19.0.0/docs/Prelude.html#v:min
[haddock-max]: https://hackage.haskell.org/package/base-4.19.0.0/docs/Prelude.html#v:max
[haddock-print]: https://hackage.haskell.org/package/base-4.19.0.0/docs/Prelude.html#v:print
[haddock-traverse_]: https://hackage.haskell.org/package/base-4.19.0.0/docs/Data-Foldable.html#v:traverse_
[haddock-iterate]: https://hackage.haskell.org/package/base-4.19.0.0/docs/Prelude.html#v:iterate
[haddock-round]: https://hackage.haskell.org/package/base-4.19.0.0/docs/Prelude.html#v:round
[haddock-fmap]: https://hackage.haskell.org/package/base-4.19.0.0/docs/Prelude.html#v:fmap
[haddock-drop]: https://hackage.haskell.org/package/base-4.19.0.0/docs/Prelude.html#v:drop


## Modelling the fund balance

A **stateful computation** will work out how much money we need in
the fund at the start of each year.  Specifically, we use the
[`State`][haddock-State] type from the [*mtl*][hackage-mtl] library
to define the computation.

[hackage-mtl]: https://hackage.haskell.org/package/mtl
[haddock-State]: https://hackage.haskell.org/package/mtl-2.3.1/docs/Control-Monad-State-Lazy.html#t:State

The model takes into account the contribution amount and the growth
factor.  To do this is has to work backwards in time.  Each `step`
of the computation takes the schooling fee for that year, and the
state value tracks the required balance of the fund.

We define the `step` function, which considers what happens to the
fund over one year.  It must first [`subtract`][haddock-subtract]
the contribution from the required balance.  This excludes the
contribution from the (presumed) growth over the year; a simplifying
assumption that is reasonable *over the long-term*.  We also ensure
the balance will not be negative.  We intend to **exhaust the fund**
when fees are paid in the final year, and a negative balance will
spoil the subsequent calculations.  The [`modify`][haddock-modify]
function applies its argument (the subtraction) to **modify the
state value**.

[haddock-subtract]: https://hackage.haskell.org/package/base-4.19.0.0/docs/Prelude.html#v:subtract
[haddock-modify]: https://hackage.haskell.org/package/mtl-2.3.1/docs/Control-Monad-State-Lazy.html#v:modify

```haskell
step contrib fee = do
  modify (max 0 . subtract contrib)
```

Next we *divide* the balance (state value) by the **growth rate**,
to obtain a nominal value of the fund at the start of the year:

```haskell
  modify (/ 1.08)
```

Finally, we have to **pay the fees** at (or near) the start of the
year.  So we must *increase* the required balance by that amount.
Some schools offer a discount for full year payment up-front.  This
model applies a discount of 5%, but this is another area where the
model could be parameterised.

```haskell
  modify (+ (fee * 0.95))
```

All together the `step` function contains a few simple operations.
At the end it returns the state value via the [`get`][haddock-get]
function.  This will enable us to see how the value of the fund
changes year by year.

```haskell
step contrib fee = do
  modify (max 0 . subtract contrib)
  modify (/ 1.08)
  modify (+ (fee * 0.95))
  get
```

[haddock-get]: https://hackage.haskell.org/package/mtl-2.3.1/docs/Control-Monad-State-Lazy.html#v:get

Now that we have the `step` function, we can
[`traverse`][haddock-traverse] the list of year costs and apply the
`step` to each element.  Like `traverse_`, `traverse` applies an
action to each element of a container, but instead of discarding the
results it replaces each element of the container with the "return
value" of the action.

We have to start at the final year and work backwards, so first
[`reverse`][haddock-reverse] the list, then `traverse` it:

```haskell
go :: Double -> State Double [Double]
go contrib =
  traverse (step contrib) (reverse feesCombinedInflated)
```

[haddock-traverse]: https://hackage.haskell.org/package/base-4.19.0.0/docs/Prelude.html#v:traverse
[haddock-reverse]: https://hackage.haskell.org/package/base-4.19.0.0/docs/Prelude.html#v:reverse

This is the first time I have shown a **type signature** in this
whole post!  The `go` function takes the contribution amount and
returns a *state computation* whose *state variable* (the required
balance) is a real number (`Double`) and whose *output* is a list of
numbers (the required balance at the start of each year).

To actually **run the state computation** we use
[`evalState`][haddock-evalState], whose arguments are the state
computation and an *initial state*.  Our initial state is *$0*,
because we intend to exhaust the fund when we pay for the final year
of schooling.

```haskell
model contrib = fmap round (reverse (evalState go 0))
```

::: note

`evalState` yields the final *output* of the state computation,
discarding the state variable.  If you instead want the final value
of the *state variable*, use [`execState`][haddock-execState].
[`runState`][haddock-runState] yields the *(state var, output)*
pair.

:::

The result of `model` is a list of the required fund balance at the
start of each year, in order.  The first value is the initial
balance required for the given yearly contribution amount.

[haddock-evalState]: https://hackage.haskell.org/package/mtl-2.3.1/docs/Control-Monad-State-Lazy.html#v:evalState
[haddock-execState]: https://hackage.haskell.org/package/mtl-2.3.1/docs/Control-Monad-State-Lazy.html#v:execState
[haddock-runState]: https://hackage.haskell.org/package/mtl-2.3.1/docs/Control-Monad-State-Lazy.html#v:runState


## Running the model

Let's see what the model tells us for various contribution amounts.
First, let's just pick a number, say *$10,000*.

```
λ> traverse_ print (model 10000)
43542
57025
71587
87314
104299
106929
108984
110379
92373
71086
46161
36165
24183
```

The final year's value is `24183`.  That will *always* be the final
value, regardless of contribution amount, because that is what the
final fee payment will be.

As for the required starting value, for a *$10,000* yearly
contribution we would need to start the fund with *$43,542*.  But
what if you don't have any money to start the fund?  With a bit of
trial and error I found that a yearly contribution of *$15,778* is
enough (note the start value for the *second* year):

```
λ> traverse_ print (model 15778)
0
15776
32816
51220
71095
76847
82273
87309
73235
56195
35857
30815
24183
```

Finally, how much would you need if you wanted the fund to be
completely passive—no further contributions after the initial
amount?

```
λ> traverse_ print (model 0)
118903
128415
138688
149783
161766
158993
155213
150306
125494
96857
63994
45424
24183
```

*$118,903*.  Not many people have that kind of money at their
immediate disposal.  But if you do, or if you get a big windfall,
you could "set and forget" an investment for your children's
schooling, or other long-term financial objective.


## Possible model enhancements or variations

There are several ways the model could be improved, or tweaked to
suit the circumstances or preferences of the investor.

You could consider **increasing the contribution over time** to
adjust for expected income growth.  There are a several ways to do
it.  One way is to pass the inflation factor to the step function
and apply it to the base contribution amount there.  Another way is
to precompute the annual inflated contribution, `zip` it with the
inflated fee list, and `traverse` the `step` function over the
*(contribution, fee)* pairs.  I leave it to the reader to play
around, if interested.

Another obvious area for improvement is that the model is hardcoded
for exactly two children.  Enhancing it to **handle different
numbers of children** would be a good exercise.  `zipWith` will no
longer cut it for combining fees.  Furthermore, schools can have
diverse discount structures for multiple children (e.g. second child
10% off, 25% for third child, and so on).  So the discount structure
should be parameterised.  If you want to improve your skill working
with lists in Haskell, this would be a good exercise.

Other areas for improvement include dollar-cost averaging the yearly
contribution amount, or step functions for different payment
frequencies (e.g. quarterly / per term).  Both of these tasks would
be good **practice with `State` computations**.
