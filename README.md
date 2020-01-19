# Ordinals

This is just my quick take at test-driven development in Haskell with the amazing [QuickCheck](http://book.realworldhaskell.org/read/testing-and-quality-assurance.html) library. The project includes an implementation for ordinal arithmetic in Cantor normal form and some real-world test cases, mainly

* [Goodstein sequences](https://en.wikipedia.org/wiki/Goodstein%27s_theorem)
* [the Hydra game](http://math.andrej.com/2008/02/02/the-hydra-game/)

## Usage

The module `Ordinals` exports a single type `Ordinal` that implements `Num` for arithmetic, `Ord`, `Eq` and `Show`. We can thus work with finite ordinals just by

    1, 2, fromInteger 42 :: Ordinal

Transfinite ordinals can be created through the exponential function `w :: Ordinal -> Ordinal` in base *omega*. So

    w 1 

is just *omega*. We can continue from there 

    w 1 < w 1 + 1 < w 1 + 2 < ... w 1 + w 1 == w 1 * 2 < w 1 * 2 + 1 < ... w 2 < w 2 + 1 ...

up to towers of powers like `w (w (w 5))` 

Have fun exploring ordinal arithmetic, like

    2 + w 1 == w 1

and

    (w 3 + w 2) * 4 == w 3 * 4 + w 2

## Testing

Testing in Haskell is straightforward. Create an instance of `Arbitrary` for your type and write the properties you want to verify


    test_mult_associative :: Ordinal -> Ordinal -> Ordinal -> Bool
    test_mult_associative a b c = ((a * b) * c) == (a * (b * c))
    
    test_distributive :: Ordinal -> Ordinal -> Ordinal -> Bool
    test_distributive a b c = (a * (b + c)) == (a * b + a * c)

At the end, just

    quickCheck test_mult_associative

and `QuickCheck` will scrutinize the given property with a variety of automatically generated test cases of different complexities.



# License

To the extent possible under law, the author(s) have dedicated all copyright and related and neighboring rights to this software to the public domain worldwide. This software is distributed without any warranty. See <[https://creativecommons.org/publicdomain/zero/1.0/]>(https://creativecommons.org/publicdomain/zero/1.0/)