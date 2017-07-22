-- --------------------------------------------------------------- [ Arity.idr ]
-- Module      : Data.Arity
-- Description : 
-- --------------------------------------------------------------------- [ EOH ]
module Data.Arity

import Data.Vect
import Data.Permutation

%default total

%access public export

arityVect : (Vect n Type) -> (b : Type) -> Type
arityVect Nil b = b
arityVect (t::ts) b = t -> (arityVect ts b)

f : arityVect [Int, Int, Int] Bool 
f m n p = m + n == p
