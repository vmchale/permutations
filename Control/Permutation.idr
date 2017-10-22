module Control.Permutation

import Data.List
import Data.Vect
import Control.Lens.At

%default total

%access public export

private
(>>) : (Monad m) => m a -> m b -> m b
(>>) a a' = a >>= (const a')

-- FIXME consider using *two* constructors and forcing them to use `n` as an argument?

||| This is something like `Vector k a`, except we restrict ourselves to only 1,...,n for `Permutation n`.
data Permutation : Nat -> Type where
  Nil : Permutation Z
  (::) : Fin (S n) -> Permutation n -> Permutation (S n)

||| This extends 'Monoid' by defining an inverse for every element.
interface (Monoid t) => Group (t : Type) where
  inverse : t -> t

-- FIXME
interface (Ixed t) => Action t where
  act  : t -> Permutation n -> t

||| This is essentially a group action. Given a permutation, we apply it to the vector.
||| We do not require that the vector's elements come from a set of size n.
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

getElem : Permutation n -> Fin n -> Fin n
getElem p n = index n $ toVector p

orbit : Permutation (S n) -> Fin (S n) -> Stream (Fin (S n))
orbit p {n} i = i :: go i where
  go : Fin (S n) -> Stream (Fin (S n))
  go j = next :: go next where
    next : Fin (S n)
    next = getElem p j

finOrbit : Permutation (S n) -> Fin (S n) -> List (Fin (S n))
finOrbit p {n} i = nub $ take n (orbit p i)

cycles : Permutation (S n) -> Maybe (List (List (Fin (S n))))
cycles p {n} = nub . map sort . map (finOrbit p) . enumFromTo 0 <$> (natToFin n (S n))

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
cycle {n=S _} = FZ :: cycle

-- if sigma=(143)(27689) then sigma=(13)(14)(29)(28)(26)(27)
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

-- also proving something is a homo. ALSO apparently all injections are catamorphisms, but I don't think that's useful here

private
injects : Permutation m -> Permutation n -> Bool
injects {m} {n} _ _ = m < n

||| Inject ↪
--injection : (p1 : Permutation m) -> { auto p : injects p1 p2 = True }Y> -> (p2 : Permutation n)
--injection p = fill (size p)

||| The permutation π_ij
export
pi : Fin n -> Fin n -> Permutation n
pi (FS j) (FS k) = FZ :: pi j k
pi (FS j) FZ = FS j :: fill j
pi FZ (FS k) = FS k :: fill k
pi FZ FZ = id

||| FIXME this is dumb.
compose : Permutation n -> Permutation n -> Permutation n
compose x Nil = x
compose Nil y = y
compose x y = ?f x y

-- TODO: apply a permutation to a vector, then use that to find an inverse?
invert : Permutation n -> Permutation n
invert Nil = Nil
invert x = ?f x

implementation Semigroup (Permutation n) where
  (<+>) = compose

implementation Monoid (Permutation n) where
  neutral = id

implementation Group (Permutation n) where
  inverse = invert

-- TODO Permutations are a type of lens!! And should be viewed as such.
