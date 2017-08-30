module Test.Spec

import Specdris.Spec
import Control.Permutation
import Data.Vect

export
specSuite : IO ()
specSuite = printLn $ trivial
  where trivial : Permutation 4
        trivial = pi (FS 0) (FS 2)
