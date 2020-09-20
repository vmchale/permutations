module Data.Group

%default total

%access public export

||| This extends 'Monoid' by defining an inverse for every element.
interface (Monoid t) => Group t where
  inverse : t -> t

||| (Positive) integer exponentiation.
exp : (Group g) => (n : Nat) -> g -> g
exp n g = (head . drop n) (generate g) where
  generate g = h where
    h = assert_total $ neutral :: map (<+> g) h

||| Whether a group element is idempotent
idempotent : (Eq g, Semigroup g) => g -> Bool
idempotent x = x == (x <+> x)

||| Commutator of two elements.
commutator : (Group g) => g -> g -> g
commutator a b = inverse a <+> inverse b <+> a <+> b
