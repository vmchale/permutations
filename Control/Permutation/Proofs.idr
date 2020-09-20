module Control.Permutation.Proofs

import Control.Permutation.Mod
import Data.Nat

||| Proof that (n + 1)! >= n!
export
factorialIncr : (n : Nat) -> LTE (factorial n) (factorial (S n))
factorialIncr Z = lteRefl
factorialIncr n = lteAddRight (factorial n)

||| Proof that n! >= 1
export
factorialLTE : (n : Nat) -> LTE 1 (factorial n)
factorialLTE Z = lteRefl
factorialLTE (S k) = lteTransitive (factorialLTE k) (factorialIncr k)
