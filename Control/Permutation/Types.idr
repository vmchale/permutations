module Control.Permutation.Types

import Data.List
import Data.Vect
import Data.Group
import Data.Vect.Lazy

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

debug : Permutation n -> String
debug Nil = "Nil"
debug (p::ps) = show (finToNat p) ++ " :: " ++ (debug ps)

||| This is essentially a group action. Given a permutation, we apply it to a vector.
sigma : Permutation n -> Vect n a -> Vect n a
sigma _ [] = []
sigma (p::ps) (x::xs) = insert (sigma ps xs) p
  where
    insert : Vect n a -> Fin (S n) -> Vect (S n) a
    insert l FZ = x::l
    insert [] _ = [x]
    insert (e::es) (FS k) = e :: insert es k

toVector : Permutation n -> Vect n (Fin n)
toVector {n} p = sigma p (sequential n)
  where
    sequential : (n : Nat) -> Vect n (Fin n)
    sequential Z = []
    sequential (S k) = FZ :: map FS (sequential k)

indices : Permutation n -> Vect n (Fin n)
indices [] = Nil
indices (p :: ps) = p :: map (thin p) (indices ps)
  where
    thin : Fin (S n) -> Fin n -> Fin (S n)
    thin FZ i = FS i
    thin _ FZ = FZ
    thin (FS i) (FS j) = FS (thin i j)

private
delete : Fin (S n) -> Permutation (S n) -> Permutation n
delete FZ (j :: p) = p
delete {n=Z} (FS _)  _ = Nil
delete {n=S _} (FS i) (j :: p) = (either lifter id $ strengthen j) :: delete i p
  where
    lifter (FS k) = k
    lifter FZ = FZ

private
compose : Permutation n -> Permutation n -> Permutation n
compose Nil p = p
compose (i :: p) p' = index i (indices p') :: compose p (delete i p')

export
invert : Permutation n -> Permutation n
invert Nil = Nil
invert p@(i :: is) = index (i' p) (indices p) :: delete (i' p) p
  where
    i' p = index i (indices p)

implementation Show a => Show (Lazy a) where
  show (Delay x) = show x

implementation Eq a => Eq (Lazy a) where
  (==) (Delay x) (Delay y) = x == y

implementation Show (Fin n) where
  show FZ = "0"
  show (FS k) = show $ (finToNat k) + 1

implementation Semigroup (Permutation n) where
  (<+>) = compose

implementation Monoid (Permutation n) where
  neutral = id

implementation Group (Permutation n) where
  inverse = invert
