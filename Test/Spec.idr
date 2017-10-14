module Test.Spec

import Specdris.Spec
import Control.Permutation
import Control.Permutation.Arity
import Data.Vect

export
specSuite : IO ()
specSuite =
  spec $ do
    describe "trivial" $
      it "should permute a vector" $
        toVector trivial `shouldBe` [0, 3, 2, 1]
  where trivial : Permutation 4
        trivial = pi (FS 0) (FS 2)
