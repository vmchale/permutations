module Data.Group

import Data.Stream

%default total

||| This extends 'Monoid' by defining an inverse for every element.
interface (Monoid t) => Group t where
  inverse : t -> t

||| Stream of elements starting at some given element.
generate : (Group g) => g -> Stream g
generate g1 = neutral :: map (<+> g1) (generate g1)

||| (Positive) integer exponentiation.
exp : (Group g) => (n : Nat) -> g -> g
exp n g = Data.Stream.head (drop n (generate g))

||| Whether a group element is idempotent
idempotent : (Eq g, Semigroup g) => g -> Bool
idempotent x = x == (x <+> x)

||| Commutator of two elements.
commutator : (Group g) => g -> g -> g
commutator a b = inverse a <+> inverse b <+> a <+> b
