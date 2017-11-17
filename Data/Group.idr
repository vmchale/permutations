module Data.Group

%default total

%access public export

||| This extends 'Monoid' by defining an inverse for every element.
interface (Monoid t) => Group (t : Type) where
  inverse : t -> t

||| Whether a group element is idempotent
idempotent : (Eq g, Semigroup g) => g -> Bool
idempotent x = x == (x <+> x)

||| Commutator of two elements.
commutator : (Group g) => g -> g -> g
commutator a b = inverse a <+> inverse b <+> a <+> b
