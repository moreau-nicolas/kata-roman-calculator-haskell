module Roman.CalculatorSpec (spec) where

import Test.Hspec (describe, it, shouldBe, Spec )

import Prelude hiding ((+))
import Roman (RomanDigit(..), (+))


spec :: Spec
spec = do
  describe "Roman Calculator" $ do
    it "I + I should be II" $ do
        [I] + [I] `shouldBe` [I,I]
    it "I + II should be III" $ do
        [I] + [I,I] `shouldBe` [I,I,I]
    it "I + III should be IV" $ do
        [I] + [I,I,I] `shouldBe` [I,V]
    it "IV + III should be VII" $ do
        [I,V] + [I,I,I] `shouldBe` [V,I,I]
    it "V + I should be VI" $ do
        [V] + [I] `shouldBe` [V,I]
    it "I + V should be VI" $ do
        [I] + [V] `shouldBe` [V,I]
    it "I + VI should be VII" $ do
        [I] + [V,I] `shouldBe` [V,I,I]
    it "XI + V should be XVI" $ do
        [X,I] + [V] `shouldBe` [X,V,I]
    it "III + I should be IV" $ do
        [I,I,I] + [I] `shouldBe` [I,V]
    it "III + II should be V" $ do
        [I,I,I] + [I,I] `shouldBe` [V]
    it "IV + IV should be VIII" $ do
        [I,V] + [I,V] `shouldBe` [V,I,I,I]
    it "V + V should be X" $ do
        [V] + [V] `shouldBe` [X]
    it "V + VI should be XI" $ do
        [V] + [V,I] `shouldBe` [X,I]
    it "VI + VI should be XII" $ do
        [V,I] + [V,I] `shouldBe` [X,I,I]
    it "XV + LII should be LXVII" $ do
        [X,V] + [L,I,I] `shouldBe` [L,X,V,I,I]
    it "XVIII + XVIII should be XXXVI" $ do
        [X,V,I,I,I] + [X,V,I,I,I] `shouldBe` [X,X,X,V,I]
    it "XV + XXV should be XL" $ do
        [X,V] + [X,X,V] `shouldBe` [X,L]
    it "CL + CCL should be CD" $ do
        [C,L] + [C,C,L] `shouldBe` [C,D]
    it "CC + CC should be CD" $ do
        [C,C] + [C,C] `shouldBe` [C,D]
    it "L + L should be C" $ do
        [L] + [L] `shouldBe` [C]
    it "D + D should be M" $ do
        [D] + [D] `shouldBe` [M]
    it "MCMLXXXIV + MCMLXXXVII should be MMMCMLXXI" $ do
        [M,C,M,L,X,X,X,I,V] + [M,C,M,L,X,X,X,V,I,I] `shouldBe` [M,M,M,C,M,L,X,X,I]
    it "IIXX + II should be XX" $ do
        [I,I,X,X] + [I,I] `shouldBe` [X,X]
    it "IIIXX + III should be XX" $ do
        [I,I,I,X,X] + [I,I,I] `shouldBe` [X,X]


