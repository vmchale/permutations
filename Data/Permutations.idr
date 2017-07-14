-- -------------------------------------------------------- [ Permutations.idr ]
-- Module      : Data.Permutations
-- Description : 
-- --------------------------------------------------------------------- [ EOH ]
module Data.Permutations

import Data.Vect

%access export

data Permutation : Nat -> Type -> Type -> Type where
  Bij : a -> Vect k a -> Vect k a -> Permutation k a _

getSize : Permutation k (Vect k a) (Vect k a) -> Nat
getSize (Bij _ v _) = length v
