module Data.Group

import Data.Stream

||| This extends 'Monoid' by defining an inverse for every element.
public export
interface (Monoid t) => Group t where
  inverse : t -> t

||| Stream of elements starting at some given element.
public export
generate : (Monoid g) => g -> Stream g
generate g1 = neutral :: map (<+> g1) (generate g1)

||| (Positive) integer exponentiation.
public export
exp : (Monoid g) => (n : Nat) -> g -> g
exp n g = Prelude.head (drop n (generate g))

||| Whether a group element is idempotent
public export
total
idempotent : (Eq g, Semigroup g) => g -> Bool
idempotent x = x == (x <+> x)

||| Commutator of two elements.
public export
total
commutator : (Group g) => g -> g -> g
commutator a b = inverse a <+> inverse b <+> a <+> b
