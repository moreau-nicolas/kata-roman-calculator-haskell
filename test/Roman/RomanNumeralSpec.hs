module Roman.RomanNumeralSpec (spec) where

import Test.Hspec (describe, it, shouldBe, Spec)

import Roman (RomanDigit(..), romanNumeral)


spec :: Spec
spec = do
  describe "romanNumeral" $ do
    it "should parse I" $ do
        romanNumeral "I" `shouldBe` [I]
    it "should parse  II" $ do
        romanNumeral "II" `shouldBe` [I,I]
    it "should parse III" $ do
        romanNumeral "III" `shouldBe` [I,I,I]
    it "should parse IV" $ do
        romanNumeral "IV" `shouldBe` [I,V]
    it "should parse MDCLXVI" $ do
        romanNumeral "MDCLXVI" `shouldBe` [M,D,C,L,X,V,I]
