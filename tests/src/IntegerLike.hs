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
  let prop p = forAll $ suchThat arbitrary p
      unary = prop $ not . isLuaNative
      binary = prop $ not . all isLuaNative
      unary' f = prop $ \a -> (not . isLuaNative $ a) && f (a, a)
      binary' f = prop $ \ab -> (not . all isLuaNative $ ab) && f ab

  describe "relational operators" $ do
    let ops = [ ("==", True, (==))
              , ("~=", False, (/=))
              , ("<", False, (<))
              , ("<=", True, (<=))
              , (">", False, (>))
              , (">=", True, (>=))
              ]
    flip mapM_ ops $ \(oplua, refl, op) -> describe oplua $ do
      it (printf "should %s be reflexive (by reference)" (be refl)) $ properly $ unary $ \a -> do
        s <- runLua $ do
          "a" `bind` a
          return' $ printf "a %s a" oplua
        s `shouldBe` op a a
      it (printf "should %s be reflexive (by value)" (be refl)) $ properly $ unary $ \a -> do
        s <- runLua $ do
          "a0" `bind` a
          "a1" `bind` a
          return' $ printf "a0 %s a1" oplua
        s `shouldBe` op a a
      it "should adhere to the reference implementation" $ properly $ binary $ \(a, b) -> do
        s <- runLua $ do
          "a" `bind` a
          "b" `bind` b
          return' $ printf "a %s b" oplua
        s `shouldBe` op a b

  describe "binary operators" $ do
    let ops = [ ("+", True, (+))
              , ("-", False, curry $ \(a, b) -> max 0 (a - b))
              , ("*", True, (*))
              ]
    flip mapM_ ops $ \(oplua, comm, op) -> describe oplua $ do
      it "should adhere to the reference implementation" $ properly $ binary $ \(a, b) -> do
        s <- runLua $ do
          "a" `bind` a
          "b" `bind` b
          return' $ printf "a %s b" oplua
        s `shouldBe` op a b

      it "should behave as expected when called with the same operand" $ properly $ unary $ \a -> do
        s <- runLua $ do
          "a" `bind` a
          return' $ printf "a %s a" oplua
        s `shouldBe` op a a

      when comm $ it "should be commutative" $ properly $ binary $ \(a, b) -> do
        s <- runLua $ do
          "a" `bind` a
          "b" `bind` b
          return' $ printf "a %s b == b %s a" oplua oplua
        s `shouldBe` True

  describe "partial binary operators" $ do
    let ops = [ ("//", (\(_, b) -> b /= 0), div)
              , ("%", (\(_, b) -> b /= 0), rem)
              ]
    flip mapM_ ops $ \(oplua, def, op) -> describe oplua $ do
      describe "when defined" $ do
        it "should adhere to the reference implementation" $ properly $ binary' def $ \(a, b) -> do
          s <- runLua $ do
            "a" `bind` a
            "b" `bind` b
            return' $ printf "a %s b" oplua
          s `shouldBe` op a b

        it "should behave as expected when called with the same operand" $ properly $ unary' def $ \a -> do
          s <- runLua $ do
            "a" `bind` a
            return' $ printf "a %s a" oplua
          s `shouldBe` op a a
