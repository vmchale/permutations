-- --------------------------------------------------------------- [ Arity.idr ]
-- Module      : Data.Arity
-- Description : 
-- --------------------------------------------------------------------- [ EOH ]
module Data.Arity

import Data.Vect
import Data.Permutation

%default total

%access public export

infixr 9 .**

arityVect : (Vect n Type) -> (b : Type) -> Type
arityVect Nil b = b
arityVect (t::ts) b = t -> (arityVect ts b)

f : arityVect [Int, Int, Int] Bool 
f m n p = m + n == p

(.**) : (d -> e) -> arityVect [a, b, c] d -> arityVect [a, b, c] e
(.**) f g = \x, y, z => f (g x y z)

fuckInputs : (p : Permutation 3) -> arityVect [a, b, c] d -> arityVect (sigma p [a, b, c]) d
fuckInputs (FZ::FZ::FZ::Nil) f x y z = f x y z -- id
fuckInputs ((FS (FS FZ))::(FS FZ)::FZ::Nil) f x y z = f z y x -- (132)
fuckInputs ((FS FZ)::(FS FZ)::FZ::Nil) f x y z = f y z x -- (123)
fuckInputs ((FS FZ)::FZ::FZ::Nil) f x y z = f y x z -- (12)
fuckInputs (FZ::(FS FZ)::FZ::Nil) f x y z = f x z y -- (13)
fuckInputs ((FS (FS FZ))::FZ::FZ::Nil) f x y z = f z x y -- (312)

--fuckInputsHard : (p : Permutation 3) -> (d -> e) -> arityVect [a, b, c] d -> arityVect (sigma p [a, b, c]) e
--fuckInputsHard p h f = h .** (fuckInputs p f)
