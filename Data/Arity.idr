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

fuckInputs : arityVect [a, b, c] d -> arityVect (reverse [a, b, c]) d
fuckInputs f x y z = f z y x

fuckInputsHard : (p : Permutation 3) -> arityVect [a, b, c] d -> arityVect (sigma p [a, b, c]) d
fuckInputsHard (FZ::FZ::FZ::Nil) f x y z = f x y z
