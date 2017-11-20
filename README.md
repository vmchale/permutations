# permutations

This is a library providing a type-safe implementation of permutations.
To my knowledge, It is the only such library outside of
Coq.

## Installation

```bash
 $ idris --install permutations.ipkg
```

You may need to grab the latest Idris compiler from HEAD.

## Use

### Tips

The most useful thing this library provides is a `Group` instance for
permutations. You can multiply two elements with `<+>`, invert with `inverse`,
an so on.

The [lazy\_matrices](https://hub.darcs.net/vmchale/lazy_matrices) package uses
permutations to compute determinants with the [Laplace
formula](https://www.encyclopediaofmath.org/index.php?title=Determinant).

#### Notation

The `Show` instance for `Permutation` uses cycle notation. You can read more
[here](http://dlmf.nist.gov/26.13) if you find it confusing.

### Documentation

You can find documentation
[here](https://vmchale.github.io/permutations/index.html).
