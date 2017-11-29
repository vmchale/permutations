module Control.Permutation.Proofs

import Control.Permutation.Types
import Control.Permutation.Mod

%access public export
%default total

private
induct : (P : Nat -> Type) ->
         (P Z) ->
         ((k : Nat) -> P k -> P (S k)) ->
         (x : Nat) ->
         P x
induct P base step Z = base
induct P base step (S k) = step k (induct P base step k)

private
lteMultRight : (n : Nat) -> (m : Nat) -> LTE n (n * (S m))
lteMultRight Z k = lteRefl
lteMultRight (S k) m = l
  where
    l : LTE (S k) (S (plus m (mult k (S m)))) 
    l = ?hole_again -- lteAddRight

private
factorialLemma : (k : Nat) -> LTE (factorial k) (factorial (S k)) -> LTE (factorial (S k)) (factorial (S (S k)))
factorialLemma k prf = ?idk_hole

||| Proof that (n + 1)! >= n!
private
factorialIncr : (n : Nat) -> LTE (factorial n) (factorial (S n))
factorialIncr n = induct (\n => LTE (factorial n) (factorial (S n))) lteRefl factorialLemma n

||| Proof that n + 1 - 1 = n for n >= 1
trivial : {auto smaller : LTE 1 n} -> S (n - 1) = n
trivial {n=S Z} = Refl
trivial = ?holey_hole

||| Proof that n! >= 1
export
factorialLTE : (n : Nat) -> LTE 1 (factorial n)
factorialLTE Z = lteRefl
factorialLTE (S k) = lteTransitive (factorialLTE k) (factorialIncr k)
