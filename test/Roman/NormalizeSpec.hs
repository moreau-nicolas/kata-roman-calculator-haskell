module Roman.NormalizeSpec (spec) where

import Test.Hspec (describe, it, shouldBe, Spec)

import Roman (RomanDigit(..), normalize)


spec :: Spec
spec = do
  describe "normalize" $ do
    it "III should be normalized to III" $ do
        normalize [I,I,I] `shouldBe` [I,I,I]
    it "IIII should be normalized to IV" $ do
        normalize [I,I,I,I] `shouldBe` [I,V]
    it "IIIII should be normalized to V" $ do
        normalize [I,I,I,I,I] `shouldBe` [V]
    it "CCCC should be normalized to CD" $ do
        normalize [C,C,C,C] `shouldBe` [C,D]
    it "CCCCC should be normalized to D" $ do
        normalize [C,C,C,C,C] `shouldBe` [D]
    it "CCCCCC should be normalized to DC" $ do
        normalize [C,C,C,C,C,C] `shouldBe` [D,C]
    it "XXXX should be normalized to XL" $ do
        normalize [X,X,X,X] `shouldBe` [X,L]
    it "VV should be normalized to X" $ do
        normalize [V,V] `shouldBe` [X]
    it "VVV should be normalized to XV" $ do
        normalize [V,V,V] `shouldBe` [X,V]
    it "LL should be normalized to C" $ do
        normalize [L,L] `shouldBe` [C]
    it "LLL should be normalized to CL" $ do
        normalize [L,L,L] `shouldBe` [C,L]
    it "DD should be normalized to M" $ do
        normalize [D,D] `shouldBe` [M]
    it "DDD should be normalized to MD" $ do
        normalize [D,D,D] `shouldBe` [M,D]
    it "XVV should be normalized to XX" $ do
        normalize [X,V,V] `shouldBe` [X,X]
    it "XXXVV should be normalized to XL" $ do
        normalize [X,X,X,V,V] `shouldBe` [X,L]
    it "CCCLL should be normalized to CD" $ do
        normalize [C,C,C,L,L] `shouldBe` [C,D]
    it "DCCCC should be normalized to CM" $ do
        normalize [D,C,C,C,C] `shouldBe` [C,M]
    it "MMMDD should be normalized to MMMM" $ do
        normalize [M,M,M,D,D] `shouldBe` [M,M,M,M]
    it "MMMM should be normalized to MMMM" $ do
        normalize [M,M,M,M] `shouldBe` [M,M,M,M]
    it "MMMMM should be normalized to MMMMM" $ do
        normalize [M,M,M,M,M] `shouldBe` [M,M,M,M,M]
    it "IIIIIIIIIIIIIIIIIII should be normalized to XIX" $ do
        normalize [I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I] `shouldBe` [X,I,X]
    it "IIIIIIIIIIIIIIIIIIII should be normalized to XX" $ do
        normalize [I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I] `shouldBe` [X,X]
    it "IIIIIIIIIIIIIIIIIIIIIIII should be normalized to XXIV" $ do
        normalize [I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I,I] `shouldBe` [X,X,I,V]
    it "VVVV should be normalized to XX" $ do
        normalize [V,V,V,V] `shouldBe` [X,X]
    it "VVVVV should be normalized to XXV" $ do
        normalize [V,V,V,V,V] `shouldBe` [X,X,V]
    it "VVVVVVVVV should be normalized to XLV" $ do
        normalize [V,V,V,V,V,V,V,V,V] `shouldBe` [X,L,V]
