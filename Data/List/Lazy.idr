module Data.List.Lazy

%default total

%access public export

data LazyList : (a : Type) -> Type where
  Nil : LazyList a
  (::) : (x : Lazy a) -> Lazy (LazyList a) -> LazyList a

implementation Foldable LazyList where
  foldr c n Nil = n
  foldr c n (x :: xs) = c x (foldr c n xs)

implementation Eq a => Eq (LazyList a) where
  (==) x y = toList x == toList y

implementation Show a => Show (LazyList a) where
  show = show . toList

implementation Functor LazyList where
  map f Nil = Nil
  map f (x :: xs) = f x :: map f xs

repeat : a -> LazyList a
repeat x = assert_total $ x :: repeat x

zipWith : (f : a -> b -> c) -> (l : LazyList a) -> (r : LazyList b) -> LazyList c
zipWith f []      (y::ys) = []
zipWith f (x::xs) []      = []
zipWith f []      []      = []
zipWith f (x::xs) (y::ys) = f x y :: zipWith f xs ys

implementation Semigroup (LazyList a) where
  (<+>) x Nil = x
  (<+>) Nil x = x
  (<+>) (x::xs) y = x :: (xs <+> y)

implementation Monoid (LazyList a) where
  neutral = Nil

implementation Applicative LazyList where
  pure = (:: Nil)
  (<*>) fs xs = concat $ zipWith map fs $ map (\x => repeat x) xs

implementation Monad LazyList where
  (>>=) x f = concat $ map f x

filter : (a -> Bool) -> LazyList a -> LazyList a
filter _ Nil = Nil
filter p (x :: xs) = if p x then x :: filter p xs else filter p xs

sort : (Ord a) => LazyList a -> LazyList a
sort Nil = Nil
sort (x :: xs) = s <+> [x] <+> b
  where
    s = filter (<= x) xs
    b = filter (> x) xs

elem : (Eq a) => a -> LazyList a -> Bool
elem y Nil = False
elem y (x :: xs) = if y == x then True else y `elem` xs

nub : (Eq a) => LazyList a -> LazyList a
nub = foldr (\x, acc => if x `elem` acc then acc else x :: acc) Nil

length : LazyList a -> Nat
length = foldr (\_, acc => S acc) 0
