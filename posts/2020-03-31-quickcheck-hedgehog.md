----
tags: testing
----

# Migrating from QuickCheck to Hedgehog: mixed results

I've known about
[Hedgehog](https://hackage.haskell.org/package/hedgehog) from its
early days.  Having used QuickCheck before Hedgehog arrived, I am
familiar with QuickCheck's pain points, in particular the lack of
automatic shrinking and the frustrations of its type class-driven
approach.   Hedgehog was designed to—and does—solve these problems.

Yet, being already a user of QuickCheck in most of my Haskell
projects, I did not feel a need to make the switch.  "Some day, but
not today".  And finally, the day has come.  In this post I will
explain the catalyst and the results of the switch including a
surprising and (for me) detrimental behavioural difference between
QuickCheck and Hedgehog.

## Background

[*purebred-email*](https://hackage.haskell.org/package/purebred-email)
is a comprehensive mail processing library.  It has plenty of tests,
example and property-based, including serialiser/parser round-trip
tests.  Email has a 7-bit (ASCII) wire format; there are various
mechanisms for including 8-bit data in messages.  For including
8-bit data in header values, [RFC
2047](https://tools.ietf.org/html/rfc2047) defines the
`encoded-word` mechanism.  Serialised Unicode data in the `To` and
`From` headers can look something like this:

```
MIME-Version: 1.0
From: =?utf-8?B?0JDQu9C40YHQsA==?= <alice@example.com>
To: =?utf-8?Q?Riob=C3=A1rd_Baker?= <bob@example.net>
Content-Transfer-Encoding: 7bit
Content-Disposition: inline
Content-Type: text/plain; charset=us-ascii

Hello, Bob!
```

Recently someone filed an
[issue](https://github.com/purebred-mua/purebred-email/issues/50)
that *purebred-email* was not encoding the display name part of
email addresses properly.  It was indeed the case that raw UTF-8
data was appearing in the rendered message.  Alongside a candidate
fix I took the opportunity to add a round-trip QuickCheck property
that would test serialisation and re-parsing of an email address
with arbitrary mailboxes in the `From` header.  The display name
part of the mailbox could include Unicode characters.  The property,
and some related generators, were defined as follows:

```haskell
prop_messageRoundTrip :: Property
prop_messageRoundTrip = forAll genMailbox $ \mailbox ->
  let
    l = headerFrom defaultCharsets
    msg = set l [mailbox] (createTextPlainMessage "Hello")
  in
    (view l <$> parse (message mime) (renderMessage msg))
    == Right [mailbox]

genDomain :: Gen Domain
genDomain = DomainDotAtom <$> genDotAtom

genDotAtom :: Gen (NonEmpty B.ByteString)
genDotAtom = fromList <$> listOf1 fragment
  where
  fragment = B.pack <$> listOf1 atext
  atext = arbitrary `suchThat` isAtext

genLocalPart :: Gen B.ByteString
genLocalPart = fold . intersperse "." <$> genDotAtom

genAddrSpec :: Gen AddrSpec
genAddrSpec = AddrSpec <$> genLocalPart <*> genDomain

genMailbox :: Gen Mailbox
genMailbox = Mailbox <$> arbitrary <*> genAddrSpec
```

Note that I explicitly define and directly use the generators.
Defining `Arbitrary` instances for these types was too restrictive.
I also felt it would be difficult to implement accurate and useful
shrinking heuristics for these domain objects.

And this is where the fun began.


## From QuickCheck to Hedgehog

By default, QuickCheck checks properties 100 times with random
inputs.  I had a hunch that for this property, 100 was not enough,
so I overrode the default to check `prop_messageRoundTrip` 10,000
times.  And hey, what do you know, QuickCheck found a bug:

```
    message round trip with From header:        FAIL (3.89s)
      *** Failed! Falsified (after 1090 tests):
      Mailbox (Just "\r\n\617309\990252F\SO{\36170\EOTE\rjxHg\NUL\375521\40710\878394\812276%\273790\USU2
\NAK\DC1\FS=K#\SO\SYNAIA\37830\617194jP\201749\1025883cV3\98741\RS\\=\r5H'^o\109453\925605\247522<\775764
\1029678I%\\.{e\1108491R9MT\942184KJ\EOTj\1080860\SOwB\162509\ENQ") (AddrSpec "Ad+j3dRo21+%_fQ|_^1SnUQhwq
p7z2zimd}{|KaGI#F^4kIk9jO.%C6SUZ=$vNfiprz|O4j.bE1rAcfFP&9/C3L.OT-QDI=De5kF$qD=4dfNa`ReRBSX`J-PB}xaYIbEoCm
IMsE{}.tvrjkuw=6=--dX|33Y/v+~kNbhGguS.xCM7bX2+|kH=lvRY3Z#QyGUb++ZvMI66e^3/yMM`K30Lud_kGnm`4cfdKv.VGLCF#.{
!uVXd*|KI&A2oEa2awZ6oIrAUckzM9%qgz}A|1O9Sd~WdJ*plt?3$OI=WmU7B`.hlpz1'|6JtBuYoCpWwsL7m+d7vX?xDOSjbr/0FOxr|
2N?p$nQ**P*R6pj2HfubS6VW-lap33WI^.MPYiQcw3SUfKVo0eU~zX3W#xCG4fxV~sswK&O2E9.0NXG_4zOJyWvv=-7`2e*jydY=sQeET
h9=_~ypqW9D.xarK{XeP`#9gUJ2O!Jg7pb$t037mO3rjAwDKxs/VZZy}1{3NIzzuyl!cF~sCQAzS---6HtLvmEgYhWjijVH.Svl`sV#-y
/}B8gUFK'l?Bnoj^pU-MI.Vrw{WFXLbZ09GW!cdtPObmhz}?v8xzz+LR`U?cBP!zuI=iRTK}_m#9PGJNH6WZcn3u4td-8y{rj_r^DKY{q
K*w+kK.%x.cLIek|fzQ9dJLpaVI|fJx{!-~sFO-}Q_?7F#-naFR4s7#7=Y_U-%HXhKsK1qJC.{P3*F7==MCX6sGU^q5sh9M*L0fFF9knT
h^A6ZAXYLS$BX}/31sU|}+Q!cdj#hGFRqcjp~IoHw|JGKK}/6Y?E%+cop.*&`rJM87aH!Nph&6pMU-U+Z3o-7L.HAoHW#-vfadyy+AA?O
9LI2hU5xfkGAwz/T-7$Wr='x7-'+}AZ6yUGB.!&nq7`ViGdFM#Er0}yJtWFRi#/zNMf!_gtBitQH{=G}PefcLcPF^%S3K6qqsl.?NqTE0
e&$%GHS.7AGCSviyx*-Bq|hA6+'!#{foj!$i.7evxp3cwoST#Bbz4CmJeAyUHMqsgnbW&fG08gOoBqBIuda#$q}G3e1G8f!b5w^_A}C.!
=HPqzE1.4enC.QrCTo*~PE%`}5Q-/PPNXw|3!%^*s$YaFdvjPVegSJ4vuJAa6o{A-5pfW#-IT1}r?dw'y.`SzR&X#G%zFF%d%/wWS1cyZ
f%WXox_j^qew1?4P!uBqb&&XxoLk*d*gOg-Uh/WG~#3Zqq`ZSw#}Cu'2&JK.83YmO!v0g$bNSixtQpV!-##0VHRC#flu~_`.*w^/HWm7s
oMfEy^Sh082g3*R!vlPFCWZ7wk%J}GLxZcNut!%8p?+Y|A9}'Za5-{kkGMMH|G%dRcbL|.IQDP1VzCY'p'h+~{`|_Aba*Fl5ccRJhWyGC
T._4#I%7TXR0FOH0+T%u67Per++r?kePkhWExZ6co{i#oBkZ6U&XHbrxV5Xy{#Dk#0|%0CgWy*{8t*+.&|}?/*i/Zp{LYJgWISJNqNkKx
nZtPo04/MTH'.$R7ck6kvg2&vsCwqPLo`H%E*y$7`sw~9d*#53M4%KKw|qlN1h`*P2#&=6U{exw^'I^|?/`=-WOip/Cn{5`h'1X.l3B^N
G?UP6lbO-%Dh7.|obTf3fH252SHx62hpeM8*Va}2Gv+5z`Pnun?EDL.Wx-EkiTsfsc05E3X`%WAtO3mgoc$TfFrX'JsSa_+k$/Hw8Mou=
hh^7B2206}PSj%~W966_-VB.4w2.khd0'i^3Ie!#u%Lo0U54HctD1eJdHR2q.mZKa6b$|UT26dz$|eMg}$Qo+k`ron_nT~#32L9`P8y7.
ft7tN{Lf}S&85#|32O6IJA3jE4gCz=ZN^1&Z9uDaUl|KJ3jGPHD.R~c~v!-rXY$Dx{MsJ'f~gz&4*UgWe|aO=8BQZF$gk_U1W_oy/TVJ2
C9W%.Y%#hgwGxU?-g&PhoG+RfflLU-ad-m-%/`m+n-c+^/8^9kj&dG={0+bgkw0xrJh{PF1cKkWbN&.k1jtJXaqp442SHMenuYf2&R3VV
~~{Q}.iPFaih}fhC6yP%^'+AugNXl?jh*pLJb+U_0pz3+`QP3w9SS!EnH!0m`NZzbQ4%Moa.N}qMwRJnb#L{BE2d?+*-rmdL!YnV7E^EO
tbQS~dmT5AVEOeHhz8*IchVaLl{Xvdubm0&XYO6GSzH|gO%#wO~Kl{h.AZ2N#$r&wKk-msV8ybJ9$LQ.*2=u+0pws3+8bdFdJJ&E/%Qa=
BmN?rl~QZfxQq~|Vz*#s_ZTs4Kp^%2{0!cJOSI9|Dk.WL%H0pRgY.Ht?h{Xz3L!A^-AIcT{=U+YNssZnaJ%%=}8Ylp6zO.3L5SNnB&6G+
b`6{e#p5565M4WDriUH4!IByU?US{TkQf5c8WP3}XirW1RJDT~-e=u!v4mj63QBaYYr6.rx*NJ8olK+Xu3Z4&YI8C2?x_6`.M%6G.TX!U
n0VOsNM8~Bss8XKA|.i9k}99EeOx%JqIXc+{u.Gu9Ns=d1}HasDj^IzFDSa5$SzeK.4hwf#a1Fcewp_PXOC-VKJuKz.8mG+Kjy4Pgxwm4
`rv5lP%8{jEfjhsj9t$zsp4mb85J_ZyXbyFgWo2t1RT'-ReGCSh.ypHp!C/3UiFGJ_/#A|YSL?{#5b~`U0Y$xVvh9taFi#'NJcIy5H'3D
*I.?KJ9ngID*l2M.Cg`*~yCdu{pXYG**8slMmHXA.zgw.9v-jzj'xti{E|+Pg~nT+b+w6BkJTx53|^Q6U^Ll#.COxJYvKowVc3Oy_l!6^
2'4YtmFZHYL-e1U|e2c+aAMJTFNKbeEUMfCpjEX%$$?oS}^.9F33J$_1ALwi*+MNU&qvsLQ/WI^UnCd4+.Z/%xt.fMCbDzRJcZFN#=Qvi
_C6Z|4LCf&0a4iM-Dg^|&YVr8wIfV0z$.1gQXg3__l2%ir-vXIQ{0pf}k!Ejx+#L|j_X6DBTit`s{2.Cx2d63gz&9IdkD6klHwx_{vKCu
D9{}Pb7GvTqL4c6sAJ'H'XaxJa8-3.-WAewPr0h.|SwcuIFliH5-Ro8zigOb=92^ZR%aM8B5I$wbNrU}XB4#dYYAIlBP1Cx~?Fw7BqIV_
ri^.z_znwUBWdhYK7^JYG0$F#Bk-rc%rfy*XiazKA2OuMs.k" (DomainDotAtom ("Ut||VyF|OIoq`9h`6!`_nL|s+b5OLb}VM!Qe'+
1" :| ["UtT9C?7!stiF&i","u","52IO'0S9wrvodlpL`}M^N#K|6Hliu!hd`sk7t&wwD0S%H#ZWcvIf+ZCn{C$4Q38NZ/{hn2GdL0/l
ZExv","wP{kH9SF2v?hH`81GI{aJyGmje3d1o`DRS4r4'rMzXs"])))
      Use --quickcheck-replay=822386 to reproduce.
```

Without shrinking, the counterexample was a wall of text.  It was
good to know that there was a problem, but I didn't even attempt to
make any sense of it.  I knew that this was the moment.  It was time
to unleash the Hedgehog.

Switching to Hedgehog was a simple mechanical translation.  The
updated definitions follow.

```haskell
prop_messageRoundTrip :: Property
prop_messageRoundTrip = property $ do
  from <- forAll genMailbox
  let
    l = headerFrom defaultCharsets
    msg = set l [from] (createTextPlainMessage "Hello")
  (view l <$> parse (message mime) (renderMessage msg))
    === Right [from]

genDomain :: Gen Domain
genDomain = DomainDotAtom <$> genDotAtom

genDotAtom :: Gen (NonEmpty B.ByteString)
genDotAtom = Gen.nonEmpty (Range.linear 1 5) fragment
  where
  fragment = Gen.utf8 (Range.linear 1 20) atext
  atext = Gen.filter isAtext Gen.ascii

genLocalPart :: Gen B.ByteString
genLocalPart = fold . intersperse "." <$> genDotAtom

genAddrSpec :: Gen AddrSpec
genAddrSpec = AddrSpec <$> genLocalPart <*> genDomain

genMailbox :: Gen Mailbox
genMailbox =
  Mailbox
  <$> Gen.maybe (Gen.text (Range.linear 0 100) Gen.unicode)
  <*> genAddrSpec
```

As you can see there are no structural changes.  Indeed, several of
the definitions did not change at all (except that the name `Gen`
now refers to a different type).

I ran the tests again, expecting Hedgehog to find the bug and,
thanks to integrated shrinking, present me with a digestable
counterexample.  But the tests passed.  Even after 10,000 iterations
it could not detect the bug:

```
message round trip with From header:             OK (4.75s)
  ✓ message round trip with From header passed 10000 tests.
```

## Generator bias

Hedgehog's inability to find a counterexample surprised me, and
several other people in `#bfpg`.  The search for answers soon led me
to the source code, where all was laid bare.  Hedgehog's
`Gen.unicode` has a uniform distribution over all Unicode
characters:

```haskell

-- | Generates a Unicode character, excluding noncharacters
--   and invalid standalone surrogates:
--   @'\0'..'\1114111' (excluding '\55296'..'\57343',
--    '\65534', '\65535')@
--
unicode :: (MonadGen m) => m Char
unicode =
  let
    s1 =
      (55296, enum '\0' '\55295')
    s2 =
      (8190, enum '\57344' '\65533')
    s3 =
      (1048576, enum '\65536' '\1114111')
  in
    frequency [s1, s2, s3]
```

Whereas QuickCheck's `Char` generator, although it can generate any
Unicode character, has a heavy bias to the ASCII codepoints (0–127):

```haskell
instance Arbitrary Char where
  arbitrary =
    frequency
      [(3, arbitraryASCIIChar),
       (1, arbitraryUnicodeChar)]
```

After discovering this difference I implemented an equivalent
Hedgehog generator to use instead of `Gen.unicode`, and updated
`genMailbox` to use it:

```haskell
unicodeCharAsciiBias :: Gen Char
unicodeCharAsciiBias =
  Gen.frequency [(3, Gen.ascii), (1, Gen.unicode)]

genMailbox :: Gen Mailbox
genMailbox =
  Mailbox
  <$> Gen.maybe (Gen.text (Range.linear 0 100)
                 unicodeCharAsciiBias)
  <*> genAddrSpec
```

## Shrink to win

This time Hedgehog found the counterexample.  The automatic
shrinking produced a minimal counterexample and Hedgehog presented
its findings:

```
message round trip with From header:                  FAIL (47.81s)
 ✗ message round trip with From header failed at tests/Message.hs:106:3
   after 866 tests and 69 shrinks.

       ┏━━ tests/Message.hs ━━━
   100 ┃ prop_messageRoundTrip :: Property
   101 ┃ prop_messageRoundTrip = property $ do
   102 ┃   from <- forAll genMailbox
       ┃   │ Mailbox (Just "\r\n") (AddrSpec "!" (DomainDotAtom ("!" :| [])))
   103 ┃   let
   104 ┃     l = headerFrom defaultCharsets
   105 ┃     msg = set l [from] (createTextPlainMessage "Hello")
   106 ┃   (view l <$> parse (message mime) (renderMessage msg)) === Right [from]
       ┃   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
       ┃   │ ━━━ Failed (- lhs) (+ rhs) ━━━
       ┃   │   Right [
       ┃   │       Mailbox
       ┃   │ -       Just "=?us-ascii?Q? ?="
       ┃   │ +       Just "\r\n"
       ┃   │         AddrSpec "!" (DomainDotAtom ("!" :| []))
       ┃   │     ]
```

Isn't the presentation—that neatly formatted diff of the data
structure—just *gorgeous*?!  It is easy to see the problem:
*purebred-email* did not round-trip an email address correctly when
then display name was (perhaps more generally, *contained*) a
carriage return followed by a newline/line feed (CRLF).  `"\r\n"
!@!` is a pretty bonkers email address but the types and grammar do
permit it so *purebred-email* must handle it correctly.

## Probabilities

So now we can see why QuickCheck was able to find a counterexample
and Hedgehog (when using `Gen.unicode`) was not.  It is a matter of
probability distribution.  The probability of selecting CR followed
by LF from a uniform distribution of all 1112062 unicode characters
is 1 in 1236681891844, whereas for the 75% ASCII distribution
(noting that the other 25% for all Unicode characters *also*
includes the ASCII codepoints) is 2782747776649 over
81047184463888384 or roughly 0.0000433491.

Note that 0.0000433491 is a bit less than half of 1 in 10000.  We
expect then that if we were executing 10000 tests, the framework
would find this counterexample less than half the time.  But this
probability is for two-character sequences.  The probability of a
CRLF subsequence occuring in a longer string of randomly selected
characters is, intuitively, much greater.  But my probability-fu is
not strong enough to work all that out.  As it happens, with the
ASCII-biased distribution both QuickCheck and Hedgehog usually find
the counterexample somewhere around the 1000th test (but sometimes
much sooner).


## Shrinking performance

Automatic shrinking is a joy.  But Hedgehog's shrinking is slow
compared to QuickCheck.  In the example above, it took almost a
whole minute, most of which was the shrinking (compare with the
earlier `Gen.unicode` example which tested the property 10,000 times
in 4.7 seconds).

I don't see this has a problem: if it takes a long time to find a
minimal counterexample, so be it.  The tradeoff is worth it.  And it
is only the shrinking that is slow.  If your tests are passing (and
hopefully they do, most of the time) then there is no penalty.

While I was discussing these things, one person shared with me that
Hedgehog ate all their memory during shrinking, and crashed.  So the
slowness might be due to space usage (at least in part).  I didn't
experience any crashes (yet) but it was prudent to share this
anecdote.  Your mileage may vary.


## Conclusion

Hedgehog is great.  It solves the major pain points of QuickCheck.
Automated shrinking for all generators is a killer feature, but it
is computationally (and/or space) expensive, and might eat all your
memory and crash (I have not experienced this myself).  The pretty
output with a nicely formatted diff of the data structure makes it
easier to comprehend the counterexamples than QuickCheck's
`Show`-based output.

Converting from QuickCheck to Hedgehog is a breeze; a simple
mechanical translation.  But **do not blindly convert**.  I would
probably never have found this bug if I had already converted
*purebred-email* to Hedgehog, because of a critical difference in
the distribution of one of the generators.  When you are converting,
pay careful attention to the behaviour of the generators, especially
if they produce character or string types.

The issue I experienced comes down to a lack of documentation.
Arguably QuickCheck is the bad guy in this tale: the non-uniform
distribution should have been documented.  But it would be good for
*all* generators or `Arbitrary` instances to say something about
their distribution, even if it's just *"uniform distribution"*.

I always intended to start using Hedgehog, and expected that it
would be a gradual transition.  At time of writing, QuickCheck and
Hedgehog are happily coexisting in the *purebred-email* test suite.
From now on any new test modules I write will probably use Hedgehog,
and older modules will be converted any time I bump against
QuickCheck's shrinking or type class-related rough edges.
