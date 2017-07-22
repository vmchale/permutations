-- --------------------------------------------------------------- [ Sized.idr ]
-- Module      : Data.Sized
-- Description : 
-- --------------------------------------------------------------------- [ EOH ]
module Data.Sized

import Data.Vect

%default total

%access public export

interface Sized (t : Type) where
  size : t -> Nat

implementation Sized (Vect n a) where
  size v = length v

