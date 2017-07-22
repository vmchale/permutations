-- --------------------------------------------------------- [ Permutation.idr ]
-- Module      : Data.Permutation
-- Description : 
-- --------------------------------------------------------------------- [ EOH ]
module Data.Permutation

import Data.Vect

%default total

%access public export

private
(>>) : (Monad m) => m a -> m b -> m b
(>>) a a' = a >>= (const a')

-- IDEALLY, at the type level, we would also like to enforce the distinction that the vector
-- has all distinct elements. 
-- 
-- Failing that, a proof of some aspects would be good.

||| This is something like `Vector k a`, except we restrict ourselves to only 1,...,n for `Permutation n`.
data Permutation : Nat -> Type where
  Nil : Permutation Z
  (::) : Fin (S n) -> Permutation n -> Permutation (S n)

interface Group (t : Type) where
  identity : t
  multiply : t -> t -> t
  inverse : t -> t

||| This is essentially a group action. Given a permutation, we apply it to the vector.
||| We do not require that the vector's elements come from a set of size n.
σ : Permutation n -> Vect n a -> Vect n a
σ [] [] = []
σ (p::ps) (x::xs) = insert (σ ps xs) p
  where insert : Vect n a -> Fin (S n) -> Vect (S n) a
        insert l FZ = x::l
        insert [] (FS i) = [x]
        insert (e::es) (FS k) = e :: insert es k

toVector : Permutation n -> Vect n (Fin n)
toVector {n} p = σ p (count n)
  where count : (n : Nat) -> Vect n (Fin n)
        count Z = []
        count (S k) = FZ :: map FS (count k)

implementation Show (Fin n) where
  show FZ = "0"
  show (FS k) = show $ (finToNat k) + 1

implementation Show (Permutation n) where
  show p = show (toVector p)

-- Also nice: take a string, return a permutation! Or also "fromVector" would be v useful.

id : Permutation n
id {n=Z} = []
id {n=S _} = FZ :: id

reverse : Permutation n
reverse {n=Z} = []
reverse {n=S _} = last :: reverse

-- e.g. (1234) for S_4
cycle : Permutation n
cycle {n=Z} = []
cycle {n=S _} = ?s

getSize : Permutation n -> Nat
getSize Nil = Z
getSize (x::xs) = S (getSize xs)

mangleTypes2 : Permutation 2 -> Type -> Type -> Vect 2 Type
mangleTypes2 (FZ :: (FZ :: Nil)) t1 t2 = [t1, t2]
mangleTypes2 ((FS FZ) :: (FZ :: Nil)) t1 t2 = [t2, t1]

-- | Type-safe manipulation of any number of arguments
mangle2 : (p: Permutation 2) -> (a -> b -> c) -> (index 0 (mangleTypes2 p a b) -> index 1 (mangleTypes2 p a b) -> c)
mangle2 (FZ :: (FZ :: Nil)) f = \x, y => f x y
mangle2 ((FS FZ) :: (FZ :: Nil)) f = \x, y => f y x

-- if σ=(143)(27689) then σ=(13)(14)(29)(28)(26)(27)
-- ideally, we should have a proof that the resulting list contains only transpositions as well.
decompose : Permutation n -> List (Permutation n)
decompose p = pure p

private
fill : Fin n -> Permutation n
fill FZ = id
fill (FS k) = FS (zeros k) :: fill k
  where zeros : Fin m -> Fin m
        zeros FZ = FZ
        zeros (FS _) = FZ

-- injections? ↪ : Permutation m -> Permutation n but w/ constraints?
-- also proving something is a homo. ALSO apparently all injections are catamorphisms??

-- function to decompose a permutation into its contitutent parts? i.e. cycles/etc.
export
π : Fin n -> Fin n -> Permutation n
π (FS j) (FS k) = FZ :: π j k
π (FS j) FZ = FS j :: fill j
π FZ (FS k) = FS k :: fill k
π FZ FZ = id

||| FIXME this is dumb.
compose : Permutation n -> Permutation n -> Permutation n
compose x Nil = x
compose Nil y = y
compose x y = ?f x y

||| FIXME don't use this.
invert : Permutation n -> Permutation n
invert Nil = Nil
invert x = ?f x

implementation Group (Permutation n) where
  identity = id
  multiply = compose
  inverse = invert
