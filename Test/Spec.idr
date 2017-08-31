module Test.Spec

import Specdris.Spec
import Control.Permutation
import Control.Permutation.Arity
import Data.Vect

export
specSuite : IO ()
specSuite = do 
    printLn trivial
    printLn arguments
  where trivial : Permutation 4
        trivial = pi (FS 0) (FS 2)

        arguments : Bool
        arguments = g 3 1 4
