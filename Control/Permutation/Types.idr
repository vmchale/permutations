module Control.Permutation.Types

import Data.List
import Data.Vect
import Data.Group

%default total

%access public export

||| This is something like `Vector k a`, except we restrict ourselves to only 1,...,n for `Permutation n`.
data Permutation : Nat -> Type where
  Nil : Permutation Z
  (::) : Fin (S n) -> Permutation n -> Permutation (S n)

implementation Eq (Permutation n) where
  (==) Nil Nil = True
  (==) (x :: xs) (y :: ys) = x == y && xs == ys

private
id : Permutation n
id {n=Z} = []
id {n=S _} = FZ :: id

||| This is essentially a group action. Given a permutation, we apply it to a vector.
sigma : Permutation n -> Vect n a -> Vect n a
sigma _ [] = []
sigma (p::ps) (x::xs) = insert x p (sigma ps xs)
  where
    insert : a -> Fin (S n) -> Vect n a -> Vect (S n) a
    insert y FZ ys = y::ys
    insert y _ [] = [y]
    insert y (FS k) (z::zs) = z :: insert y k zs

toVector : Permutation n -> Vect n (Fin n)
toVector {n} p = sigma p (sequential n)
  where
    sequential : (n : Nat) -> Vect n (Fin n)
    sequential Z = []
    sequential (S k) = FZ :: map FS (sequential k)

private
indices : Permutation n -> Vect n (Fin n)
indices [] = Nil
indices (p::ps) = p :: map (thin p) (indices ps)
  where
    thin : Fin (S n) -> Fin n -> Fin (S n)
    thin FZ i = FS i
    thin _ FZ = FZ
    thin (FS i) (FS j) = FS (thin i j)

private
delete : Fin (S n) -> Permutation (S n) -> Permutation n
delete FZ (_::p) = p
delete {n=Z} _ _ = Nil
delete {n=S _} (FS i) (j :: p) = (either lifter id $ strengthen j) :: delete i p
  where
    lifter : Fin (S (S n)) -> Fin (S n)
    lifter (FS k) = k
    lifter FZ = FZ

-- private b/c group interface
private
compose : Permutation n -> Permutation n -> Permutation n
compose Nil p = p
compose (i::p) p' = index i (indices p') :: compose p (delete i p')

private
invert : Permutation n -> Permutation n
invert Nil = Nil
invert p@(i :: is) = index (i' p) (indices p) :: delete (i' p) p
  where
    i' p = index i (indices p)

implementation Show (Fin n) where
  show FZ = "0"
  show (FS k) = show $ (finToNat k) + 1

implementation Semigroup (Permutation n) where
  (<+>) = compose

implementation Monoid (Permutation n) where
  neutral = id

implementation Group (Permutation n) where
  inverse = invert
