# permutations

This is a library providing a type-safe implementation of permutations.
To my knowledge, It is the only such library outside of
Coq.

It needs a couple tweaks before it's ready for general use, but everything
*should* be working correctly now.

## Installation

```bash
 $ idris --install permutations.ipkg
```

## Use

### Tips

The most useful thing this library provides is a `Group` instance for
permutations. You can multiply two elements with `<+>`, invert with `inverse`,
an so on.

#### Notation

The `Show` instance for `Permutation` uses cycle notation. You can read more
[here](http://dlmf.nist.gov/26.13) if you find it confusing.

### Documentation

You can find documentation
[here](https://vmchale.github.io/permutations/index.html).
