module Control.Permutation.Proofs

import Control.Permutation.Types
import Control.Permutation.Mod

%access public export
%default total

factorialIncr : (n : Nat) -> LTE (factorial n) (factorial (S n))
factorialIncr Z = lteRefl
factorialIncr n = ?idk_hole

factorialLTE : (n : Nat) -> LTE 1 (factorial n)
factorialLTE Z = lteRefl
factorialLTE (S k) = lteTransitive (factorialLTE k) (factorialIncr k)
