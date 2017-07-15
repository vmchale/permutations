module Test.Spec

import Specdris.Spec
import Data.Permutation
import Data.Vect

export

specSuite : IO ()
specSuite = printLn $ trivial
  where trivial : Permutation 4
        trivial = pi FZ FZ --(FS 1)

{--
specSuite : IO ()
specSuite = spec $ do
  describe "something trivial" $
    it "should add two numbers correctly" $
    14 + 3 `shouldBe` 17--}
