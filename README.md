# permutations

This is a library providing a type-safe implementation of permutations.

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

[Documentation](https://vmchale.github.io/permutations/index.html).
