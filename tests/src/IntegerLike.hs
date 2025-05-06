module IntegerLike where

import Control.Monad ( when )
import qualified Data.Text as T
import Text.Printf

import Data.ByteString.UTF8 as BSUTF8

import Test.Hspec
import Test.QuickCheck

import HsLua hiding ( Integer, compare, RelationalOperator )

import LuaUtils
import Utils

class IsLuaNative a where
  isLuaNative :: a -> Bool

type RelationalOperator a = ( String, Bool, a -> a -> Bool )
type BinaryOperator a = ( String, Bool, a -> a -> a )
type PartialBinaryOperator a = ( String, a -> a -> a, (a, a) -> Bool, String )

data IntegerLike a = IntegerLike { relationalOps :: [ RelationalOperator a ]
                                 , binaryOps :: [ BinaryOperator a ]
                                 , partialOps :: [ PartialBinaryOperator a ]
                                 }

instance Semigroup (IntegerLike a) where
  a <> b = IntegerLike { relationalOps = relationalOps a ++ relationalOps b
                       , binaryOps = binaryOps a ++ binaryOps b
                       , partialOps = partialOps a ++ partialOps b
                       }

instance Monoid (IntegerLike a) where
  mempty = IntegerLike [] [] []

instance IsLuaNative a => IsLuaNative (NonNegative a) where
  isLuaNative = isLuaNative . getNonNegative

syntacticOperators :: Integral a => IntegerLike a
syntacticOperators = IntegerLike { relationalOps = [ ("%a == %b", True, (==))
                                                   , ("%a ~= %b", False, (/=))
                                                   , ("%a < %b", False, (<))
                                                   , ("%a <= %b", True, (<=))
                                                   , ("%a > %b", False, (>))
                                                   , ("%a >= %b", True, (>=))
                                                   ]
                                 , binaryOps = [ ("%a + %b", True, (+))
                                               , ("%a * %b", True, (*))
                                               ]
                                 , partialOps = [ ("%a // %b", div, \(_, b) -> b /= 0, "attempt to divide by zero")
                                                , ("%a % %b", rem, \(_, b) -> b /= 0, "attempt to divide by zero" )
                                                ]
                                 }

truncatingSubtraction :: (Num a, Ord a) => IntegerLike a
truncatingSubtraction = mempty { binaryOps = [ ("%a - %b", False, \a b -> max 0 (a - b)) ] }

divremFunction :: Integral a => String -> IntegerLike a
divremFunction modname = mempty { partialOps = [ (mk "q", div, isdef, "attempt to divide by zero")
                                               , (mk "r", rem, isdef, "attempt to divide by zero")
                                               ]
                                }
  where isdef (_, b) = b /= 0
        mk v = printf "(function() local q, r = %s.divrem(%%a, %%b); return %s end)()" modname (v :: String)

binaryExpr :: String -> String -> String -> String
binaryExpr template a b = T.unpack $ T.replace "%b" (T.pack b) $ T.replace "%a" (T.pack a) (T.pack template)

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
integerLike runLua (IntegerLike { relationalOps, binaryOps, partialOps }) = do
  describe "relational operators" $ do
    flip mapM_ relationalOps $ \(expr, refl, ref) -> describe (binaryExpr expr "a" "b") $ do
      it (printf "should %s be reflexive (by reference)" (be refl)) $ properly $ unary $ \a -> do
        s <- runLua $ do
          "a" `bind` a
          return' $ binaryExpr expr "a" "a"
        s `shouldBe` ref a a
      it (printf "should %s be reflexive (by value)" (be refl)) $ properly $ unary $ \a -> do
        s <- runLua $ do
          "a0" `bind` a
          "a1" `bind` a
          return' $ binaryExpr expr "a0" "a1"
        s `shouldBe` ref a a
      it "should adhere to the reference implementation" $ properly $ binary $ \(a, b) -> do
        s <- runLua $ do
          "a" `bind` a
          "b" `bind` b
          return' $ binaryExpr expr "a" "b"
        s `shouldBe` ref a b

  describe "binary operators" $ do
    flip mapM_ binaryOps $ \(expr, comm, ref) -> describe (binaryExpr expr "a" "b") $ do
      it "should adhere to the reference implementation" $ properly $ binary $ \(a, b) -> do
        s <- runLua $ do
          "a" `bind` a
          "b" `bind` b
          return' $ binaryExpr expr "a" "b"
        s `shouldBe` ref a b

      it "should behave as expected when called with the same operand (by reference)" $ properly $ unary $ \a -> do
        s <- runLua $ do
          "a" `bind` a
          return' $ binaryExpr expr "a" "a"
        s `shouldBe` ref a a

      it "should behave as expected when called with the same operand (by value)" $ properly $ unary $ \a -> do
        s <- runLua $ do
          "a" `bind` a
          "b" `bind` a
          return' $ binaryExpr expr "a" "b"
        s `shouldBe` ref a a

      when comm $ it "should be commutative" $ properly $ binary $ \(a, b) -> do
        s <- runLua $ do
          "a" `bind` a
          "b" `bind` b
          return' $ printf "(%s) == (%s)" (binaryExpr expr "a" "b") (binaryExpr expr "b" "a")
        let _ = ref a a -- TODO?
        s `shouldBe` True

  describe "partial binary operators" $ do
    flip mapM_ partialOps $ \(expr, ref, def, msg) -> describe (binaryExpr expr "a" "b") $ do
      describe "when defined" $ do
        it "should adhere to the reference implementation" $ properly $ binary' def $ \(a, b) -> do
          s <- runLua $ do
            "a" `bind` a
            "b" `bind` b
            return' $ binaryExpr expr "a" "b"
          s `shouldBe` ref a b

        it "should behave as expected when called with the same operand (by reference)" $ properly $ unary' (\a -> def (a,a)) $ \a -> do
          s <- runLua $ do
            "a" `bind` a
            return' $ binaryExpr expr "a" "a"
          s `shouldBe` ref a a

        it "should behave as expected when called with the same operand (by value)" $ properly $ unary' (\a -> def (a,a)) $ \a -> do
          s <- runLua $ do
            "a" `bind` a
            "b" `bind` a
            return' $ binaryExpr expr "a" "b"
          s `shouldBe` ref a a

      describe "when not defined" $ do
        it "should complain" $ properly $ binary' (not . def) $ \(a, b) -> do
          Just (Exception msg') <- runLua $ do
            "a" `bind` a
            "b" `bind` b
            expectError $ dostring . BSUTF8.fromString $ "return " ++ binaryExpr expr "a" "b"
          msg' `shouldEndWith` msg
