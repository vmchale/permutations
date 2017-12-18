module Data.Indexable

import Data.Fin
import Data.Vect

%default total

%access public export

interface Indexable (n : Nat) a b where
  index : Fin (S n) -> a -> b

toVector : (Indexable n a b) => a -> Vect n b
toVector = ?holey_hole
