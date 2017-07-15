-- -------------------------------------------------------- [ Permutations.idr ]
-- Module      : Data.Permutations
-- Description : 
-- --------------------------------------------------------------------- [ EOH ]
module Data.Permutations

import Data.Vect

%default total

%access public export

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

||| This is essentially a group action. Given a permutation, we apply it to the vector.
sigma : Permutation n -> Vect n a -> Vect n a
sigma [] [] = []
sigma (p::ps) (x::xs) = insert (sigma ps xs) p
  where insert : Vect n a -> Fin (S n) -> Vect (S n) a
        insert l FZ = x::l
        insert [] (FS i) = [x]
        insert (e::es) (FS k) = e :: insert es k

toVector : Permutation n -> Vect n (Fin n)
toVector {n} p = sigma p (count n)
  where count : (n : Nat) -> Vect n (Fin n)
        count Z = []
        count (S k) = FZ :: map FS (count k)

implementation Show (Fin n) where
  show FZ = "0"
  show (FS k) = show k

implementation Show (Permutation n) where
  show p = show (toVector p)

id : Permutation n
id {n=Z} = []
id {n=S _} = FZ :: id

reverse : Permutation n
reverse {n=Z} = []
reverse {n=S _} = last :: reverse

getSize : Permutation n -> Nat
getSize Nil = Z
getSize (x::xs) = S (getSize xs)

private
fill : Fin n -> Permutation n
fill FZ = id
fill (FS k) = FS (zeros k) :: fill k
  where zeros : Fin m -> Fin m
        zeros FZ = FZ
        zeros (FS _) = FZ

-- function to decompose a permutation into its contitutent parts? i.e. cycles/etc.
export
pi : Fin n -> Fin n -> Permutation n
pi (FS j) (FS k) = FZ :: pi j k
pi (FS j) FZ = FS j :: fill j
pi FZ (FS k) = FS k :: fill k
pi FZ FZ = id

partial
compose : Permutation n -> Permutation n -> Permutation n
compose x Nil = x
compose Nil y = y
compose _ y = y

implementation Group (Permutation n) where
  identity = id
  multiply = compose

σ : Permutation n -> Vect n a -> Vect n a
σ = sigma

export
π : Fin n -> Fin n -> Permutation n
π = pi
