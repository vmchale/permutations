module Test.Spec

import Specdris.Spec
import Control.Permutation
import Control.Permutation.Arity
import Data.Vect

export
specSuite : IO ()
specSuite =
  spec $ do
    describe "isEven" $ do
      it "should work" $ do
        map isEven ps `shouldBe` [False, True]
    describe "trivial" $ do
      it "should permute a vector" $
        toVector trivial `shouldBe` [0, 3, 2, 1]
      it "work on reversed" $
        toVector reverse `shouldBe` [3, 2, 1, 0]
    describe "cycle precursor" $ do
      it "should give a fixNat function" $
        fixNat trivial 1 `shouldBe` 3
      it "should give a orbit function" $
        finOrbit trivial 1 `shouldBe` [1, 3]
    describe "show" $ do
      it "should pretty-print for n < 10" $ do
        show trivial `shouldBe` "(13)"
    describe "enumerate" $ do
      it "should work give the right number of elements for n=3" $ do
        length (toVector <$> pList) `shouldBe` 6
    describe "compose" $ do
      it "should compose nicely" $ do
        show (trivial <+> swap) `shouldBe` "(02)(13)"
    describe "invert" $ do
      it "should invert a permutation" $
        inverse trivial `shouldBe` trivial
        -- TODO property test: composing disjoint swaps should commute!
  where trivial : Permutation 4
        trivial = pi (FS 0) (FS 2)
        big : Permutation 11
        big = pi (FS 0) (FS 9)
        swap : Permutation 4
        swap = pi FZ (FS 1)
        pList : List (Permutation 3)
        pList = enumerate
        ps : List (Permutation 2)
        ps = enumerate
