module Roman.ExpandSpec (spec) where

import Test.Hspec (describe, it, shouldBe, Spec)

import Roman (RomanDigit(..), expand)


spec :: Spec
spec = do
  describe "expand (standard forms)" $ do
    it "IX should expand to VIIII" $ do
        expand [I,X] `shouldBe` [V,I,I,I,I]
    it "XIV should expand to XIIII" $ do
        expand [X,I,V] `shouldBe` [X,I,I,I,I]
    it "III should expand to III" $ do
        expand [I,I,I] `shouldBe` [I,I,I]
    it "XIII should expand to XIII" $ do
        expand [X,I,I,I] `shouldBe` [X,I,I,I]
    it "XXXIII should expand to XXXIII" $ do
        expand [X,X,X,I,I,I] `shouldBe` [X,X,X,I,I,I]
    it "XLIII should expand to XXXXIII" $ do
        expand [X,L,I,I,I] `shouldBe` [X,X,X,X,I,I,I]
    it "MCMLXXXIV should expand to MDCCCCLXXXIIII" $ do
        expand [M,C,M,L,X,X,X,I,V] `shouldBe` [M,D,C,C,C,C,L,X,X,X,I,I,I,I]
  describe "expand (other subtractive forms)" $ do
    it "IIX should expand to VIII" $ do
        expand [I,I,X] `shouldBe` [V,I,I,I]
    it "IXX should expand to XVIIII" $ do
        expand [I,X,X] `shouldBe` [X,V,I,I,I,I]
    it "IIXX should expand to XVIII" $ do
        expand [I,I,X,X] `shouldBe` [X,V,I,I,I]
    it "IIIX should expand to XVII" $ do
        expand [I,I,I,X,X] `shouldBe` [X,V,I,I]
    it "IIIC should expand to LXXXXVIII" $ do
        expand [I,I,I,C] `shouldBe` [L,X,X,X,X,V,I,I]
    it "IIC should expand to LXXXXVIIII" $ do
        expand [I,I,C] `shouldBe` [L,X,X,X,X,V,I,I,I]
    it "IC should expand to LXXXXVIIIII" $ do
        expand [I,C] `shouldBe` [L,X,X,X,X,V,I,I,I,I]
