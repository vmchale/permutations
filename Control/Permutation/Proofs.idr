module Control.Permutation.Proofs

import Control.Permutation.Types
import Control.Permutation.Mod

%access export
%default total

||| Proof that (n + 1)! >= n!
private
factorialIncr : (n : Nat) -> LTE (factorial n) (factorial (S n))
factorialIncr Z = lteRefl
factorialIncr n = lteAddRight (factorial n)

||| Proof that n! >= 1
export
factorialLTE : (n : Nat) -> LTE 1 (factorial n)
factorialLTE Z = lteRefl
factorialLTE (S k) = lteTransitive (factorialLTE k) (factorialIncr k)
