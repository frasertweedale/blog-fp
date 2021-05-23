---
tags: finance
---

# A combinator library for taxes

Doing your taxes is no fun.  But functional programming can ease the
pain.  In this post I describe and demonstrate the Haskell
[*tax*][tax-hackage] library, which provides data types and
combinators for defining taxes.

[tax-hackage]: https://hackage.haskell.org/package/tax

## What is a tax?

Wikipedia [defines][tax-definition] a tax as *a compulsory financial
charge or some other type of levy imposed on a taxpayer*.  Most
taxes have monetary "inputs and outputs" but other kinds of taxation
exist, such as the [*corvée*][corvee-definition].  Therefore *tax*
defines a type that is abstracted over its inputs and outputs:

[tax-definition]: https://en.wikipedia.org/wiki/Tax
[corvee-definition]: https://en.wikipedia.org/wiki/Corv%C3%A9e#Modern_instances


```haskell
newtype Tax b a = Tax { getTax :: b -> a }
  deriving (Semigroup, Monoid, Functor, Profunctor)
```

The `Tax b a` type is a wrapper around the function type `(b -> a)`.
Although `(->)` has all the instances we need, I found it more
ergonomic to define a new type that communicates the *intent* of the
values.  The `GeneralizedNewtypeDeriving` extension enables
automatic derivation of the following type class instances:

```haskell
instance Profunctor Tax
instance Functor (Tax a)
instance Monoid b => Monoid (Tax a b)
instance Semigroup b => Semigroup (Tax a b)
```

The `Semigroup` operation sums outputs.  The `Monoid` identity
is a 0% tax.

```haskell
λ> getTax (flat 0.1 <> flat 0.2 <> mempty) (Money 10)
$3.0
```

For convenience, *tax* exports a type synonym for taxes whose inputs
and outputs are money (of the same type).  The input is an amount
subject to taxation (often income), and the output is the tax due:

```haskell
type MoneyTax a = Tax (Money a) (Money a)
```

The [`Money`][Money-doc] type comes from the
[*dollaridoos*][dollaridoos-hackage] package.

```haskell
newtype Money a = Money a
  deriving (Eq, Ord)
```

`Money` restricts the operations that can be performed by omitting a
`Num` instance.  Dedicated functions provide the operations that
make sense for money, like *scalar* multiplication:

```haskell
($*) :: (Num a) => Money a ->       a -> Money a
(*$) :: (Num a) =>       a -> Money a -> Money a
```

`Money a` also has instances for `Semigroup` and `Monoid` when the
wrapped type has an instance of `Num`:

```haskell
instance (Num a) => Semigroup (Money a)
instance (Num a) =>    Monoid (Money a)
```

[Money-doc]: https://hackage.haskell.org/package/dollaridoos-0.1.0.0/docs/Data-Money.html#t:Money
[dollaridoos-hackage]: https://hackage.haskell.org/package/dollaridoos

All types in *tax* are abstracted over the numeric representation.
Different applications can have different requirements for
precision.  Users may want to use a type that carries additional
context, such as a currency.  Therefore *tax* lets the user choose
the numeric representation to use.


## Constructing taxes

The most basic taxes are **lump** sums, and **flat**-rate taxes:

```haskell
lump :: a -> Tax b a
lump = Tax . const

flat :: (Num a) => a -> Tax (Money a) (Money a)
flat = Tax . (*$)
```

Some other common taxation constructions include taxing the amount
**above** some threshold at a flat rate, or taxing the *whole*
amount at a flat rate when it exceeds the **threshold**.  These
functions have the same type signature (I'll show the implementation
later):

```haskell
above, threshold
  :: (Num a, Ord a)
  => Money a -> a -> Tax (Money a) (Money a)
```


## Combinators

More complex taxes can be built using a handful of
[*combinators*][comb-defn] (functions that assemble smaller
components into more complicated structures).  This section
describes the combinators provided by the *tax* package.

[comb-defn]: https://wiki.haskell.org/Combinator_pattern

Levy the **lesser** or **greater** of two taxes:

```haskell
lesserOf, greaterOf
  :: (Ord a) => Tax b a -> Tax b a -> Tax b a
lesserOf  t1 t2 = Tax (min <$> getTax t1 <*> getTax t2)
greaterOf t1 t2 = Tax (max <$> getTax t1 <*> getTax t2)
```

**Limit** the tax payable to a given amount:

```haskell
limit :: (Ord a) => a -> Tax b a -> Tax b a
limit = lesserOf . lump
```

Whereas `above` and `threshold` use flat rates, `above'` and
`threshold'` subject the taxable portion of the input to arbitrary
`Tax` computations:

```haskell
above' :: (Num b, Ord b)
       => Money b -> Tax (Money b) a -> Tax (Money b) a
above' l = lmap (\x -> max (x $-$ l) mempty)

threshold' :: (Ord b, Monoid a) => b -> Tax b a -> Tax b a
threshold' l tax =
  Tax (\x -> if x >= l then getTax tax x else mempty)
```

In `above'`, note the use of `lmap` to reduce (via the `Money`
subtraction function `($-$)`) the amount the tax is levied upon.
This is the first usage of the `Profunctor` instance, but it will
not be the last.

With `above'` and `threshold'` in hand, we now see that the
implementations of `above` and `threshold` (which apply flat-rate
taxes) are trivial:

```haskell
above, threshold
  :: (Num a, Ord a)
  => Money a -> a -> Tax (Money a) (Money a)
above     l = above'     l . flat
threshold l = threshold' l . flat
```

In real world use, I have not (so far) used `above'` or
`threshold'`; the flat rate variants sufficed.  Nevertheless, for
completeness *tax* exports the general variants.

## Examples

### Progressive tax

Many countries use *progressive taxes*, where different bands of
income are taxed at increasing flat rates.  For example, in
Australia for the 2020–21 financial year the first $18,200 is tax
free, with income between $18,200 and $45,000 taxed at 19%, then
32.5% up to $120,000, 37% up to $180,000, and 45% above $180,000.

Observe that the `Monoid` instance for `Tax` sums the outputs of
constituent taxes applied to the same input.  We can define a
function that takes a list of thresholds and rates, and constructs a
progressive tax:

```haskell
marginal :: (Num a, Ord a)
         => [(Money a, a)] -> Tax (Money a) (Money a)
marginal = foldMap (uncurry above)
```

Because of the accumulative behaviour, the rate for each band must
be the **difference** to the previous band.  The rate for the first
band is implicitly the delta to 0%.  The Australian regime can be
expressed as:

```haskell
ausTax :: (Fractional a, Ord a) => Tax (Money a) (Money a)
ausTax = marginal
  [ ( Money 18200,  0.19  - 0     )
  , ( Money 45000,  0.325 - 0.19  )
  , ( Money 120000, 0.37  - 0.325 )
  , ( Money 180000, 0.45  - 0.37  ) ]
```

The `marginal` function is useful enough that the *tax* package
provides it.

### Shade in

Australia's public health system is funded by the *Medicare Levy*.
It is currently 2% of income, but people below a certain threshold
are exempt (the threshold changes each year).  The amount above the
threshold is taxed at 10% until it reaches 2% of the input.  This
prevents a sudden jump in tax owed and eliminates a perverse
incentive to earn less than the threshold (if your income is around
that number).  The Australian Taxation Office calls this
construction a *shade in*.

Using the functions defined above and taking the lower shade in
threshold as a parameter, this tax is an elegant one-liner:

```haskell
medicareLevy
  :: (Fractional a, Ord a)
  => Money a -> Tax (Money a) (Money a)
medicareLevy l = lesserOf (above l 0.1) (flat 0.02)
```

### Tax offsets

A tax doesn't have to result in an amount owed.  Maybe your
government will *give* you some money based on your income.  Indeed
Australia has some *tax offsets* that reduce the tax paid by people
on lower incomes.

An example is the *Low Income Tax Offset*, which was previously
defined as: *$445, reduced by 1.5c for every dollar earned over
$37,000* (the current definition is more complex).  We can implement
it like so:

```haskell
lito :: (Fractional a, Ord a) => Tax (Money a) (Money a)
lito = limit mempty
  (lump (Money (-445)) <> above (Money 37000) 0.015)
```

`limit mempty` ensures that the result does not exceed $0.


### Withholding tax

Many jurisdictions collect income taxes by requiring employers to
remit a portion of employees' wages directly to the tax authority.
In Australia, the amount to *withhold* from a payment can be
determined by extrapolating the amount to an annual income,
computing the tax due, then dividing it back down to the pay period.

We can use the `Profunctor` instance to compute the amount to
withhold for different pay periods.  Think of `dimap f g` as an
adapter that modifies that data flowing in (via `f`) and out (via
`g`) of the target computation.

```haskell
allTaxes = ausTax <> medicareLevy (Money 23226) <> lito

weeklyWithholding      = dimap ($* 52) ($/ 52) allTaxes
fortnightlyWithholding = dimap ($* 26) ($/ 26) allTaxes
monthlyWithholding     = dimap ($* 12) ($/ 12) allTaxes
```

::: note

The examples above are not correct when there are 53 weekly or 27
fortnightly payments in a financial year.  Can you see how to define
the correct computation?

In the example I ignored some **rounding** rules.  I also omitted
several other tax components.  It is an example, not a complete
solution!

:::


## Conclusion

I hope you have enjoyed this tour of the *tax* library.  Of course,
most real tax systems are much more complex than the handful of
examples in this article.  But *tax* provides building blocks for
defining many kinds of taxes.

My [*tax-ato*][tax-ato-github] package builds upon *tax* to provide
types and behaviour for tax in Australia.  In additional to the
kinds of taxes described in this article it also handles captial
gains tax, [franking credits][], student loan repayments,
deductions, and other concepts.  I use it to predict and record my
own tax obligations.  If you need to perform calculations related to
tax in Australia, you might find it useful too.  It is definitely
not complete and comes with no guarantee of correctness.

[tax-ato-github]: https://github.com/frasertweedale/hs-tax-ato
[franking credits]: https://en.wikipedia.org/wiki/Dividend_imputation

One final note: oh how I wish Haskell would decouple numeric
literals from the `Num` and `Fractional` type classes.  `Money`
cannot have instances of these type classes because like other
dimensional types, it is is not [closed][] under multiplication and
division.  As a consequence, we have to lift bare numeric values
into `Money` in several places.  Separate type classes for numeric
literals would avoid this.  (`IsIntegral` and `IsRational` might be
sensible names, following the pattern of `IsString` and `IsList`).
Ultimately this is a minor inconvenience, but does add friction to
using *dollaridoos*, *tax*, and programs that use these libraries.

[closed]: https://en.wikipedia.org/wiki/Closure_(mathematics)
