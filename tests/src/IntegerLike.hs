module IntegerLike where

import Control.Monad ( when )
import Text.Printf

import Test.Hspec
import Test.QuickCheck

import HsLua hiding ( Integer, compare )

import LuaUtils
import Utils

integerLike :: ( Show op, Integral op
               , Arbitrary op
               , Peekable op, Pushable op
               )
            => RunLuaRun
            -> (op -> Bool) -- isLuaNative
            -> Spec
integerLike runLua isLuaNative = do
  let unaryily = properly . forAll (suchThat arbitrary (not . isLuaNative))
      binaryily = properly . forAll (suchThat arbitrary (not . all isLuaNative))

  describe "relational operators" $ do
    let ops = [ ("==", True, (==))
              , ("~=", False, (/=))
              , ("<", False, (<))
              , ("<=", True, (<=))
              , (">", False, (>))
              , (">=", True, (>=))
              ]
    flip mapM_ ops $ \(oplua, refl, op) -> describe oplua $ do
      it (printf "should %s be reflexive (by reference)" (be refl)) $ unaryily $ \a -> do
        s <- runLua $ do
          "a" `bind` a
          return' $ printf "a %s a" oplua
        s `shouldBe` op a a
      it (printf "should %s be reflexive (by value)" (be refl)) $ unaryily $ \a -> do
        s <- runLua $ do
          "a0" `bind` a
          "a1" `bind` a
          return' $ printf "a0 %s a1" oplua
        s `shouldBe` op a a
      it "should adhere to the reference implementation" $ binaryily $ \(a, b) -> do
        s <- runLua $ do
          "a" `bind` a
          "b" `bind` b
          return' $ printf "a %s b" oplua
        s `shouldBe` op a b

  describe "binary operators" $ do
    let ops = [ ("+", True, (+))
              , ("-", False, curry $ \(a, b) -> max 0 (a - b))
              , ("*", True, (*))
              --, ("//", False, discardByZero div)
              --, ("%", False, discardByZero mod)
              ]
    flip mapM_ ops $ \(oplua, comm, op) -> describe oplua $ do
      it "should adhere to the reference implementation" $ binaryily $ \(a, b) -> do
        s <- runLua $ do
          "a" `bind` a
          "b" `bind` b
          return' $ printf "a %s b" oplua
        s `shouldBe` op a b

      it "should behave as expected when called with the same operand" $ unaryily $ \a -> do
        s <- runLua $ do
          "a" `bind` a
          return' $ printf "a %s a" oplua
        s `shouldBe` op a a

      when comm $ it "should be commutative" $ binaryily $ \(a, b) -> do
        s <- runLua $ do
          "a" `bind` a
          "b" `bind` b
          return' $ printf "a %s b == b %s a" oplua oplua
        s `shouldBe` True
