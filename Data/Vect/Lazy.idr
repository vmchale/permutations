module Data.Vect.Lazy

import public Data.Fin

%default total

%access public export

||| This a lazy vector. It allows us to enumerate permutations efficiently.
data LazyVect : (n : Nat) -> (a : Type) -> Type where
  Nil : LazyVect Z a
  (::) : (x : Lazy a) -> (xs : Lazy (LazyVect n a)) -> LazyVect (S n) a

implementation Functor (LazyVect n) where
  map f Nil = Nil
  map f (x :: xs) = f x :: map f xs

replicate : (n : Nat) -> (x : Lazy a) -> LazyVect n a
replicate Z x = Nil
replicate (S k) x = x :: replicate k x

zipWith : (f : a -> b -> c) -> (xs : LazyVect n a) -> (ys : LazyVect n b) -> LazyVect n c
zipWith f (x :: xs) (y :: ys) = f x y :: zipWith f xs ys
zipWith f Nil _ = Nil
zipWith f _ Nil = Nil

implementation Applicative (LazyVect n) where
  pure = replicate _ . Delay
  (<*>) = zipWith apply

(++) : (xs : LazyVect m a) -> (ys : LazyVect n a) -> LazyVect (m + n) a
(++) Nil     ys = ys
(++) (x::xs) ys = x :: (xs ++ ys)

concat : (xss : LazyVect m (LazyVect n elem)) -> LazyVect (m * n) elem
concat []      = []
concat (v::vs) = v ++ concat vs

private
foldrImpl : (t -> acc -> acc) -> acc -> (acc -> acc) -> LazyVect n t -> acc
foldrImpl f e go [] = go e
foldrImpl f e go (x::xs) = foldrImpl f e (go . (f x)) xs

implementation Foldable (LazyVect n) where
  foldr f e xs = foldrImpl f e id xs

implementation (Show a) => Show (LazyVect n a) where
  show x = show (toList x)

implementation (Eq a) => Eq (LazyVect n a) where
  (==) x y = (toList x) == (toList y)

||| Get an element of a lazy vector.
index : Fin n -> LazyVect n a -> a
index FZ (x :: xs) = x
index (FS k) (x :: xs) = index k xs
