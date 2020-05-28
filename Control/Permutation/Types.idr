module Control.Permutation.Types

import Data.List
import Data.Vect
import Data.Fin
import Data.Group

%default total

infixr 7 :*

||| This is something like `Vector k a`, except we restrict ourselves to only 1,...,n for `Permutation n`.
public export
data Permutation : Nat -> Type where
  Nil : Permutation Z
  (:*) : Fin (S n) -> Permutation n -> Permutation (S n)

implementation Eq (Permutation n) where
  (==) Nil Nil = True
  (==) (x :* xs) (y :* ys) = x == y && xs == ys

private
id : {n : Nat} -> Permutation n
id {n=Z} = []
id {n=S _} = FZ :* id

debug : Permutation n -> String
debug Nil = "Nil"
debug (p:*ps) = show (finToNat p) ++ " :* " ++ (debug ps)

||| This is essentially a group action. Given a permutation, we apply it to a vector.
sigma : {n : Nat} -> Permutation n -> Vect n a -> Vect n a
sigma _ [] = []
sigma {n=S _} (p:*ps) (x::xs) = insert (sigma ps xs) p
  where
    insert : Vect n a -> Fin (S n) -> Vect (S n) a
    insert l FZ = x::l
    insert Data.Vect.Nil _ = [x]
    insert (e::es) (FS k) = e :: insert es k

export
toVector : {n : Nat} -> Permutation n -> Vect n (Fin n)
toVector {n} p = sigma p (sequential n)
  where
    sequential : (n : Nat) -> Vect n (Fin n)
    sequential Z = []
    sequential (S k) = FZ :: map FS (sequential k)

private
indices : {n : Nat} -> Permutation n -> Vect n (Fin n)
indices [] = Nil
indices (p :* ps) = p :: map (thin p) (indices ps)
  where
    thin : {n : Nat} -> Fin (S n) -> Fin n -> Fin (S n)
    thin FZ i = FS i
    thin _ FZ = FZ
    thin (FS i) (FS j) = FS (thin i j)

private
delete : {n : Nat} -> Fin (S n) -> Permutation (S n) -> Permutation n
delete FZ (j :* p) = p
delete {n=Z} (FS _)  _ = Nil
delete {n=S _} (FS i) (j :* p) = (either lifter id $ strengthen j) :* delete i p
  where
    lifter : {n : Nat} -> Fin (S n) -> Fin n
    lifter {n=Z} _ = ?hole
    lifter {n=S _} (FS k) = k
    lifter {n=S _} FZ = FZ

private
compose : {n : Nat} -> Permutation n -> Permutation n -> Permutation n
compose Nil p = p
compose (i :* p) p' = index i (indices p') :* compose p (delete i p')

export
invert : {n : Nat} -> Permutation n -> Permutation n
invert Nil = Nil
invert p@(i :* is) = index (i' p) (indices p) :* delete (i' p) p
  where
    i' : Permutation n -> Fin n
    i' p = index i (indices p)

implementation Show (Fin n) where
  show FZ = "0"
  show (FS k) = show $ (finToNat k) + 1

implementation {n : Nat} -> Semigroup (Permutation n) where
  (<+>) = compose

implementation {n : Nat} -> Monoid (Permutation n) where
  neutral = id

implementation {n : Nat} -> Group (Permutation n) where
  inverse = invert
