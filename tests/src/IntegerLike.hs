module IntegerLike ( IsLuaNative (..)

                   , Spec (..)
                   , syntacticOperators
                   , truncatingSubtraction
                   , add, mul
                   , divrem

                   , unary
                   , binary

                   , integerLike
                   ) where

import Control.Monad ( when )
import qualified Data.Text as T
import Text.Printf

import Data.ByteString.UTF8 as BSUTF8

import Test.Hspec hiding ( Spec )
import qualified Test.Hspec as Hspec
import Test.QuickCheck

import HsLua hiding ( Integer, compare, RelationalOperator )

import LuaUtils
import Utils

class IsLuaNative a where
  isLuaNative :: a -> Bool

instance IsLuaNative a => IsLuaNative (NonNegative a) where
  isLuaNative = isLuaNative . getNonNegative

class (Show a, Integral a, IsLuaNative a, Arbitrary a, Peekable a, Pushable a) => IntegerLike a where
instance (Show a, Integral a, IsLuaNative a, Arbitrary a, Peekable a, Pushable a) => IntegerLike a where

data Bin a = forall b. (Peekable b, Show b, Eq b) => MkBin (a -> a -> b)

type RelationalOperator a = ( String, Bool, a -> a -> Bool )
type BinaryOperator a = ( String, Bool, Bin a)
type PartialBinaryOperator a = ( String, Bin a, (a, a) -> Bool, String )

data Spec a = Spec { relationalOps :: [ RelationalOperator a ]
                   , binaryOps :: [ BinaryOperator a ]
                   , partialOps :: [ PartialBinaryOperator a ]
                   }

instance Semigroup (Spec a) where
  a <> b = Spec { relationalOps = relationalOps a ++ relationalOps b
                , binaryOps = binaryOps a ++ binaryOps b
                , partialOps = partialOps a ++ partialOps b
                }

instance Monoid (Spec a) where
  mempty = Spec [] [] []

syntacticOperators :: IntegerLike a => Spec a
syntacticOperators = Spec { relationalOps = [ ("%a == %b", True, (==))
                                            , ("%a ~= %b", False, (/=))
                                            , ("%a < %b", False, (<))
                                            , ("%a <= %b", True, (<=))
                                            , ("%a > %b", False, (>))
                                            , ("%a >= %b", True, (>=))
                                            ]
                          , binaryOps = [ ("%a + %b", True, MkBin (+))
                                        , ("%a * %b", True, MkBin (*))
                                        ]
                          , partialOps = [ ("%a // %b", MkBin div, \(_, b) -> b /= 0, "attempt to divide by zero")
                                         , ("%a % %b", MkBin rem, \(_, b) -> b /= 0, "attempt to divide by zero" )
                                         ]
                          }

truncatingSubtraction :: IntegerLike a => String -> Spec a
truncatingSubtraction expr = mempty { binaryOps = [ ("%a - %b", False, MkBin $ ref)
                                                  , (mk "d", False, MkBin $ ref)
                                                  , (mk "t", False, MkBin $ trunc)
                                                  , (printf "{%s(%%a, %%b)}", False, MkBin $ \a b -> (ref a b, trunc a b))
                                                  ]
                                    }
  where mk v = printf "(function() local d, t = %s(%%a, %%b); return %s end)()" expr (v :: String)
        ref a b = max 0 (a - b)
        trunc = (<)

divrem :: IntegerLike a => String -> Spec a
divrem expr = mempty { partialOps = [ (mk "q", MkBin div, isdef, msg)
                                    , (mk "r", MkBin rem, isdef, msg)
                                    , (printf "{%s(%%a, %%b)}" expr, MkBin divMod, isdef, msg)
                                    ]
                     }
  where isdef (_, b) = b /= 0
        mk v = printf "(function() local q, r = %s(%%a, %%b); return %s end)()" expr (v :: String)
        msg = "attempt to divide by zero"

add :: IntegerLike a => String -> Spec a
add expr = mempty { binaryOps = [ (printf "%s(%%a, %%b)" expr, True, MkBin (+)) ] }

mul :: IntegerLike a => String -> Spec a
mul expr = mempty { binaryOps = [ (printf "%s(%%a, %%b)" expr, True, MkBin (*)) ] }

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

integerLike :: forall op. IntegerLike op
            => RunLuaRun
            -> Spec op
            -> Hspec.Spec
integerLike runLua (Spec { relationalOps, binaryOps, partialOps }) = do
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
    flip mapM_ binaryOps $ \(expr, comm, MkBin ref) -> describe (binaryExpr expr "a" "b") $ do
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

      when comm $ it "should be commutative" $ properly $ binary $ \(a :: op, b :: op) -> do
        s <- runLua $ do
          "a" `bind` a
          "b" `bind` b
          return' $ printf "(%s) == (%s)" (binaryExpr expr "a" "b") (binaryExpr expr "b" "a")
        s `shouldBe` True

  describe "partial binary operators" $ do
    flip mapM_ partialOps $ \(expr, MkBin ref, def, msg) -> describe (binaryExpr expr "a" "b") $ do
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
