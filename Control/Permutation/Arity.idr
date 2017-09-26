module Control.Permutation.Arity

import Data.Vect
import Control.Permutation

%default total

%access public export

infixr 9 .**

-- function to apply a (sigma pi 2 1 arityVect)??
arityVect : (Vect n Type) -> (b : Type) -> Type
arityVect Nil b = b
arityVect (t::ts) b = t -> (arityVect ts b)

f : arityVect [Int, Int, Int] Bool 
f m n p = m + n == p

(.**) : (d -> e) -> arityVect [a, b, c] d -> arityVect [a, b, c] e
(.**) f g = \x, y, z => f (g x y z)

-- TODO make this work for *all* homogeneous vectors?
lemma : (p : Permutation 3) -> arityVect [a, a, a] b = arityVect (sigma p [a, a, a]) b
lemma = ?another_hole

prf : (p : Permutation 3) -> (Int -> Int -> Int -> Bool) = arityVect (sigma p [Int, Int, Int]) Bool
prf = ?hole

fuckInputs : (p : Permutation 3) -> arityVect [a, b, c] d -> arityVect (sigma p [a, b, c]) d
fuckInputs (FZ::FZ::FZ::Nil) f x y z = f x y z -- id
fuckInputs ((FS (FS FZ))::(FS FZ)::FZ::Nil) f x y z = f z y x -- (132)
fuckInputs ((FS FZ)::(FS FZ)::FZ::Nil) f x y z = f y z x -- (123)
fuckInputs ((FS FZ)::FZ::FZ::Nil) f x y z = f y x z -- (12)
fuckInputs (FZ::(FS FZ)::FZ::Nil) f x y z = f x z y -- (13)
fuckInputs ((FS (FS FZ))::FZ::FZ::Nil) f x y z = f z x y -- (312)

g : Int -> Int -> Int -> Bool
g = rewrite prf (pi 1 2) in fuckInputs (pi 1 2) f

--fuckInputsHard : (p : Permutation 3) -> (h : d -> e) -> (f : arityVect [a, b, c] d) -> arityVect (sigma p [a, b, c]) e
--fuckInputsHard p h f = h .** (fuckInputs p f) 

natInduction : (P : Nat -> Type) ->             -- Property to show
               (P Z) ->                         -- Base case
               ((k : Nat) -> P k -> P (S k)) -> -- Inductive step
               (x : Nat) ->                     -- Show for all x
               P x
natInduction P p_Z p_S Z = p_Z
natInduction P p_Z p_S (S k) = p_S k (natInduction P p_Z p_S k)
