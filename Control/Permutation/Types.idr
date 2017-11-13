module Control.Permutation.Types

import Data.List
import Data.Vect

%default total

%access public export

||| This is something like `Vector k a`, except we restrict ourselves to only 1,...,n for `Permutation n`.
data Permutation : Nat -> Type where
  Nil : Permutation Z
  (::) : Fin (S n) -> Permutation n -> Permutation (S n)

implementation Eq (Permutation n) where
  (==) Nil Nil = True
  (==) (x :: xs) (y :: ys) = x == y && xs == ys

||| This extends 'Monoid' by defining an inverse for every element.
interface (Monoid t) => Group (t : Type) where
  inverse : t -> t

id : Permutation n
id {n=Z} = []
id {n=S _} = FZ :: id

-- TODO: apply a permutation to a vector, then use that to find an inverse?
invert : Permutation n -> Permutation n
invert Nil = Nil
invert x = ?f x

||| FIXME this is dumb.
compose : Permutation n -> Permutation n -> Permutation n
compose x Nil = x
compose Nil y = y
compose (FZ :: xs) (y :: ys) = y :: (compose xs ys)
compose (x :: xs) (FZ :: ys) = x :: (compose xs ys)
compose x y = ?f x y

implementation Show (Fin n) where
  show FZ = "0"
  show (FS k) = show $ (finToNat k) + 1

implementation Semigroup (Permutation n) where
  (<+>) = compose

implementation Monoid (Permutation n) where
  neutral = id

implementation Group (Permutation n) where
  inverse = invert
-- TODO Permutations are a type of lens!! And should be viewed as such.
