module Test.Spec

import Specdris.Spec
import Control.Permutation
import Control.Permutation.Arity
import Data.Vect

export
specSuite : IO ()
specSuite =
  spec $ do
    describe "trivial" $ do
      it "should behave approximately as expected" $
        swaps (FS FZ :: FS FZ :: FZ :: Nil) `shouldBe` [(0, 2), (0, 1), (1, 2)]
      it "should permute a vector" $
        toVector trivial `shouldBe` [0, 3, 2, 1]
      it "work on reversed" $
        toVector reverse `shouldBe` [3, 2, 1, 0]
    describe "cycle precursor" $ do
      it "should give a fixNat function" $
        fixNat trivial 1 `shouldBe` 3
      it "should give a orbit function" $
        take 4 (orbit trivial 1) `shouldBe` [1, 3, 1, 3]
      it "should give a orbit function" $
        finOrbit trivial 1 `shouldBe` [1, 3]
    describe "cycles" $ do
      it "should be able to find cycles" $ do
        cycles trivial `shouldBe` [[0], [1, 3], [2]]
    describe "swaps" $ do
      it "should be able to factor a permutation as swaps" $ do
        swaps trivial `shouldBe` [(1, 3)]
    describe "show" $ do
      it "should pretty-print for n < 10" $ do
        show trivial `shouldBe` "(13)"
      {-it "should sort of work on n > 9" $ do
      show big `shouldBe` "(1,10)"-}
    describe "getAll" $ do
      it "should work give the right number of elements for n=3" $ do
        length (toVector <$> enumerate 2) `shouldBe` 6
    describe "compose" $ do
      it "should compose nicely" $ do
        toVector (compose trivial swap) `shouldBe` [2, 3, 0, 1]
  where trivial : Permutation 4
        trivial = pi (FS 0) (FS 2)
        big : Permutation 11
        big = pi (FS 0) (FS 9)
        swap : Permutation 4
        swap = pi FZ (FS 1)
