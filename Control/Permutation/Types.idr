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

||| This is essentially a group action. Given a permutation, we apply it to a vector.
sigma : Permutation n -> Vect n a -> Vect n a
sigma [] [] = []
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

private
delete : Fin (S n) -> Permutation (S n) -> Permutation n
delete FZ (j :: p) = p
delete {n=Z} (FS _)  _ = Nil
delete {n=S _} (FS i) (FZ :: p) = FZ :: delete i p
delete {n=S _} (FS i) (j :: p) = (either lifter id $ strengthen j) :: delete i p
  where
    lifter (FS k) = k
    lifter FZ = FZ

private
compose : Permutation n -> Permutation n -> Permutation n
compose Nil p = p
compose (i :: p) p' = (index i (toVector p')) :: (compose p (delete i p'))

export
invert : Permutation n -> Permutation n
invert Nil = Nil
invert p@(i :: is) = (index (i' p) (toVector p)) :: (delete (i' p) p)
  where
    i' p = Data.Vect.index i (toVector p)

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
