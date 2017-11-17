module Control.Permutation.Mod

import Prelude.Nat
import Data.List
import Data.Vect
import Control.Permutation.Types

%default total

%access public export

private
natToFin : (n : Nat) -> Fin (S n)
natToFin Z = FZ
natToFin (S k) = FS k' where k' = natToFin k

||| This permutation reverses a vector completely
reverse : Permutation n
reverse {n=Z} = []
reverse {n=S _} = last :: reverse

private
finiteL : (n : Nat) -> Vect (S n) (Fin (S n))
finiteL Z = FZ :: Nil
finiteL n@(S m) = natToFin n :: (map weaken $ finiteL m)

||| All permutations of a certain order.
export
enumerate : List (Permutation n) -- TODO vector of length n!
enumerate {n=Z} = Nil
enumerate {n=S Z} = ((FZ :: Nil) :: Nil)
enumerate {n=n@(S m)} = (::) <$> (toList $ finiteL m) <*> enumerate

||| Show where an integer is sent.
||| @p A permutation
||| @m The integer
fixNat : (p : Permutation n) -> (m : Fin n) -> Fin n
fixNat p m = index m $ (Delay $ toVector p)
  where
    index : Fin l-> Lazy (Vect l e) -> e
    index FZ     (x::xs) = x
    index (FS k) (x::xs) = index k xs

||| Orbit generated by a given element.
||| @p A permutation
||| @i The starting point
orbit : (p : Permutation (S n)) -> (i : Fin (S n)) -> Stream (Fin (S n))
orbit p {n} i = i :: go i where
  go : Fin (S n) -> Stream (Fin (S n))
  go j = next :: go next where
    next : Fin (S n)
    next = fixNat p j

||| Return the orbit of some permutation.
finOrbit : Permutation (S n) -> Fin (S n) -> List (Fin (S n))
finOrbit p {n} i = nub $ take (S n) (orbit p i)

||| Return a list of disjoint cycles given a permutation. We use this for our
||| pretty-printer.
export
cycles : Permutation (S n) -> List (List (Fin (S n)))
cycles p {n} = nub . map sort . map (finOrbit p) . enumFromTo 0 $ (natToFin n)

implementation Show (Permutation (S n)) where
  show {n} p = concatMap (go n) (cycles p)
    where
      go : (Show a) => Nat -> List a -> String
      go _ l@(_::_::_) = if n <= 9
        then "(" ++ concatMap show l ++ ")"
        else "(" ++ concat ((intersperse "," . map show) l) ++ ")"
      go _ _ = ""

private
fill : Fin n -> Permutation n
fill FZ = neutral
fill (FS k) = FS (zeros k) :: fill k
  where zeros : Fin m -> Fin m
        zeros FZ = FZ
        zeros (FS _) = FZ

||| The permutation π_ij
export
pi : Fin n -> Fin n -> Permutation n
pi (FS j) (FS k) = FZ :: pi j k
pi (FS j) FZ = FS j :: fill j
pi FZ (FS k) = FS k :: fill k
pi FZ FZ = neutral

||| swaps a permutation into a product of swaps.
export
swaps : Permutation n -> List (Permutation n)
swaps {n=Z} _ = []
swaps {n=n@(S _)} p = go overlaps p
  where
    go : (List (Fin (S n)) -> List (Permutation (S n))) -> Permutation (S n) -> List (Permutation (S n))
    go f p = (>>= f) $ cycles p
    overlaps [] = []
    overlaps [x] = []
    overlaps (x::xs@(y::ys)) = pi x y :: overlaps xs

-- TODO exterior algebras in Idris (hmm...)

{-mutual
  private
  even : Nat -> Bool
  even Z = True
  even (S k) = odd k

  private
  odd : Nat -> Bool
  odd Z = False
  odd (S k) = even k-}

||| Test whether a permutation is even.
export
isEven : Permutation n -> Bool
isEven = even . length . swaps
