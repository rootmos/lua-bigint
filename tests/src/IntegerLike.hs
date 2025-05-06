module IntegerLike where

import Control.Monad ( when )
import Text.Printf

import Data.ByteString.UTF8 as BSUTF8

import Test.Hspec
import Test.QuickCheck

import HsLua hiding ( Integer, compare )

import LuaUtils
import Utils

class IsLuaNative a where
  isLuaNative :: a -> Bool

data IntegerLike a = IntegerLike { eq :: a -> a -> Bool
                                 , divIsDefined :: (a,a) -> Bool
                                 , sub :: a -> a -> a
                                 }

instance IsLuaNative a => IsLuaNative (NonNegative a) where
  isLuaNative = isLuaNative . getNonNegative

truncatingSubtraction :: (Num a, Ord a) => IntegerLike a
truncatingSubtraction = IntegerLike { eq = (==)
                                    , divIsDefined = \(_, b) -> b /= 0
                                    , sub = \a b -> max 0 (a - b)
                                    }

unary :: (Show op, Arbitrary op, IsLuaNative op)
      => (op -> IO ()) -> Test.QuickCheck.Property
unary = forAll $ suchThat arbitrary $ not . isLuaNative

binary :: (Show op, Arbitrary op, IsLuaNative op)
       => ((op, op) -> IO ()) -> Test.QuickCheck.Property
binary = forAll $ suchThat arbitrary $ \(a, b) -> not (isLuaNative a && isLuaNative b)

binary' :: (Show op, Arbitrary op, IsLuaNative op)
        => ((op, op) -> Bool) -> ((op, op) -> IO ()) -> Test.QuickCheck.Property
binary' def = forAll $ suchThat arbitrary $ \(a, b) -> def (a, b) && not (isLuaNative a && isLuaNative b)

unary' :: (Show op, Arbitrary op, IsLuaNative op)
        => (op -> Bool) -> (op -> IO ()) -> Test.QuickCheck.Property
unary' def = forAll $ suchThat arbitrary $ \a -> def a && not (isLuaNative a)

integerLike :: ( Show op, Integral op
               , Eq op
               , IsLuaNative op
               , Arbitrary op
               , Peekable op, Pushable op
               )
            => RunLuaRun
            -> IntegerLike op
            -> Spec
integerLike runLua (IntegerLike { eq, divIsDefined, sub }) = do
  describe "relational operators" $ do
    let ops = [ ("==", True, eq) -- TODO enforce the type in a prettier way?
              , ("~=", False, (/=))
              , ("<", False, (<))
              , ("<=", True, (<=))
              , (">", False, (>))
              , (">=", True, (>=))
              ]
    flip mapM_ ops $ \(oplua, refl, ref) -> describe oplua $ do
      it (printf "should %s be reflexive (by reference)" (be refl)) $ properly $ unary $ \a -> do
        s <- runLua $ do
          "a" `bind` a
          return' $ printf "a %s a" oplua
        s `shouldBe` ref a a
      it (printf "should %s be reflexive (by value)" (be refl)) $ properly $ unary $ \a -> do
        s <- runLua $ do
          "a0" `bind` a
          "a1" `bind` a
          return' $ printf "a0 %s a1" oplua
        s `shouldBe` ref a a
      it "should adhere to the reference implementation" $ properly $ binary $ \(a, b) -> do
        s <- runLua $ do
          "a" `bind` a
          "b" `bind` b
          return' $ printf "a %s b" oplua
        s `shouldBe` ref a b

  describe "binary operators" $ do
    let ops = [ ("+", True, (+))
              , ("-", False, sub)
              , ("*", True, (*))
              ]
    flip mapM_ ops $ \(oplua, comm, ref) -> describe oplua $ do
      it "should adhere to the reference implementation" $ properly $ binary $ \(a, b) -> do
        s <- runLua $ do
          "a" `bind` a
          "b" `bind` b
          return' $ printf "a %s b" oplua
        s `shouldBe` ref a b

      it "should behave as expected when called with the same operand (by reference)" $ properly $ unary $ \a -> do
        s <- runLua $ do
          "a" `bind` a
          return' $ printf "a %s a" oplua
        s `shouldBe` ref a a

      it "should behave as expected when called with the same operand (by value)" $ properly $ unary $ \a -> do
        s <- runLua $ do
          "a" `bind` a
          "b" `bind` a
          return' $ printf "a %s b" oplua
        s `shouldBe` ref a a

      when comm $ it "should be commutative" $ properly $ binary $ \(a, b) -> do
        s <- runLua $ do
          "a" `bind` a
          "b" `bind` b
          return' $ printf "a %s b == b %s a" oplua oplua
        let _ = ref a a -- TODO?
        s `shouldBe` True

  describe "partial binary operators" $ do
    let ops = [ ("//", div, divIsDefined, "attempt to divide by zero")
              , ("%", rem, divIsDefined, "attempt to divide by zero" )
              ]
    flip mapM_ ops $ \(oplua, ref :: op -> op -> op, def :: (op,op) -> Bool, msg) -> describe oplua $ do
      describe "when defined" $ do
        it "should adhere to the reference implementation" $ properly $ binary' def $ \(a, b) -> do
          s <- runLua $ do
            "a" `bind` a
            "b" `bind` b
            return' $ printf "a %s b" oplua
          s `shouldBe` ref a b

        it "should behave as expected when called with the same operand (by reference)" $ properly $ unary' (\a -> def (a,a)) $ \a -> do
          s <- runLua $ do
            "a" `bind` a
            return' $ printf "a %s a" oplua
          s `shouldBe` ref a a

        it "should behave as expected when called with the same operand (by value)" $ properly $ unary' (\a -> def (a,a)) $ \a -> do
          s <- runLua $ do
            "a" `bind` a
            "b" `bind` a
            return' $ printf "a %s b" oplua
          s `shouldBe` ref a a

      describe "when not defined" $ do
        it "should complain" $ properly $ binary' (not . def) $ \(a, b) -> do
          Just (Exception msg') <- runLua $ do
            "a" `bind` a
            "b" `bind` b
            expectError $ dostring . BSUTF8.fromString $ printf "return a %s b" oplua
          msg' `shouldEndWith` msg
