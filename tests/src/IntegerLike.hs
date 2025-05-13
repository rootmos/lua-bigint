module IntegerLike ( IsLuaNative (..)

                   , Spec (..)
                   , relationalOperators
                   , add, mul
                   , sub, truncatingSubtraction
                   , neg
                   , divrem
                   , sign, IntegerLike.abs

                   , compare

                   , unary, unary'
                   , binary, binary'

                   , integerLike
                   ) where

import Prelude hiding ( compare )
import qualified Prelude as P

import Control.Monad ( when )
import qualified Data.Text as T
import Text.Printf

import Data.ByteString.UTF8 as BSUTF8

import Test.Hspec hiding ( Spec )
import qualified Test.Hspec as Hspec
import Test.QuickCheck

import HsLua hiding ( Integer, compare, RelationalOperator (..) )

import LuaUtils
import Utils

class IsLuaNative a where
  isLuaNative :: a -> Bool

instance IsLuaNative a => IsLuaNative (NonNegative a) where
  isLuaNative = isLuaNative . getNonNegative

class (Show a, Integral a, IsLuaNative a, Arbitrary a, Peekable a, Pushable a) => IntegerLike a where
instance (Show a, Integral a, IsLuaNative a, Arbitrary a, Peekable a, Pushable a) => IntegerLike a where

data Bin a = forall b. (Peekable b, Show b, Eq b) => MkBin (a -> a -> b)
data Un a = forall b. (Peekable b, Show b, Eq b) => MkUn (a -> b)

type RelationalOperator a = ( String, Bool, a -> a -> Bool )
type BinaryOperator a = ( String, Bool, Bin a)
type PartialBinaryOperator a = ( String, Bin a, (a, a) -> Bool, String )
type UnaryOperator a = ( String, Un a)

data Spec a = Spec { relationalOps :: [ RelationalOperator a ]
                   , binaryOps :: [ BinaryOperator a ]
                   , partialOps :: [ PartialBinaryOperator a ]
                   , unaryOps :: [ UnaryOperator a ]
                   }

instance Semigroup (Spec a) where
  a <> b = Spec { relationalOps = relationalOps a ++ relationalOps b
                , binaryOps = binaryOps a ++ binaryOps b
                , partialOps = partialOps a ++ partialOps b
                , unaryOps = unaryOps a ++ unaryOps b
                }

instance Monoid (Spec a) where
  mempty = Spec [] [] [] []

divByZeroMsg :: String
divByZeroMsg = "attempt to divide by zero"

relationalOperators :: IntegerLike a => Spec a
relationalOperators = mempty { relationalOps = [ ("%a == %b", True, (==))
                                               , ("%a ~= %b", False, (/=))
                                               , ("%a < %b", False, (<))
                                               , ("%a <= %b", True, (<=))
                                               , ("%a > %b", False, (>))
                                               , ("%a >= %b", True, (>=))
                                               ]
                             }

truncatingSubtraction :: IntegerLike a => String -> Spec a
truncatingSubtraction modname = mempty { binaryOps = [ ("%a - %b", False, MkBin ref)
                                                     , (mk "d", False, MkBin ref)
                                                     , (mk "t", False, MkBin trunc)
                                             -- TODO , car, cdr "%:sub(%b)"
                                                     , (printf "{%s.sub(%%a, %%b)}" modname, False, MkBin $ \a b -> (ref a b, trunc a b))
                                                     ]
                                       }
  where mk v = printf "(function() local d, t = %s.sub(%%a, %%b); return %s end)()" modname (v :: String)
        ref a b = max 0 (a - b)
        trunc = (<)

sub :: IntegerLike a => String -> Spec a
sub modname = mempty { binaryOps = [ ("%a - %b", False, MkBin (-))
                                   , ("%a:sub(%b)", False, MkBin (-))
                                   , (modname ++ ".sub(%a, %b)", False, MkBin (-))
                                   ]
                     }

compare :: IntegerLike a => String -> Spec a
compare modname = mempty { binaryOps = [ (modname ++ ".compare(%a, %b)", False, MkBin ref)
                                       , ("%a:compare(%b)", False, MkBin ref)
                                       ]
                         }
  where ref a b = case P.compare a b of
                    LT -> -1 :: Int
                    EQ -> 0
                    GT -> 1

divrem :: IntegerLike a => String -> Spec a
divrem modname = mempty { partialOps = [ (mk "q", MkBin quot, isdef, divByZeroMsg)
                                       , (mk "r", MkBin rem, isdef, divByZeroMsg)
                                       , (printf "{%s.divrem(%%a, %%b)}" modname, MkBin quotRem, isdef, divByZeroMsg)
                                       , ("{%a:divrem(%b)}", MkBin quotRem, isdef', divByZeroMsg)

                                       , ("%a // %b", MkBin quot, isdef, divByZeroMsg)
                                       , ("%a:div(%b)", MkBin quot, isdef', divByZeroMsg)
                                       , (modname ++ ".div(%a, %b)", MkBin quot, isdef, divByZeroMsg)

                                       , ("%a % %b", MkBin rem, isdef, divByZeroMsg)
                                       , ("%a:rem(%b)", MkBin rem, isdef', divByZeroMsg)
                                       , ("%a:mod(%b)", MkBin rem, isdef', divByZeroMsg)
                                       , (modname ++ ".rem(%a, %b)", MkBin rem, isdef, divByZeroMsg)
                                       , (modname ++ ".mod(%a, %b)", MkBin rem, isdef, divByZeroMsg)
                                       ]
                        }
  where isdef (_, b) = b /= 0
        isdef' (a, b) = (not $ isLuaNative a) && isdef (a, b)
        mk v = printf "(function() local q, r = %s.divrem(%%a, %%b); return %s end)()" modname (v :: String)

add :: IntegerLike a => String -> Spec a
add modname = mempty { binaryOps = [ ("%a + %b", True, MkBin (+))
                                   , (modname ++ ".add(%a, %b)", True, MkBin (+))
                                   , ("%a:add(%b)", True, MkBin (+))
                                   ]
                     }

mul :: IntegerLike a => String -> Spec a
mul modname = mempty { binaryOps = [ ("%a * %b", True, MkBin (*))
                                   , (modname ++ ".mul(%a, %b)", True, MkBin (*))
                                   , ("%a:mul(%b)", True, MkBin (*))
                                   ]
                     }

neg :: IntegerLike a => String -> Spec a
neg modname = mempty { unaryOps = [ ("-%a", MkUn negate)
                                  , (modname ++ ".neg(%a)", MkUn negate)
                                  , ("%a:neg()", MkUn negate)
                                  ]
                     }

sign :: IntegerLike a => String -> Spec a
sign modname = mempty { unaryOps = [ (modname ++ ".sign(%a)", MkUn signum)
                                   , ("%a:sign()", MkUn negate)
                                   ]
                      }

abs :: IntegerLike a => String -> Spec a
abs modname = mempty { unaryOps = [ (modname ++ ".%s(%%a)", MkUn Prelude.abs)
                                  , ("%a:abs()", MkUn Prelude.abs)
                                  ]
                     }

binaryExpr :: String -> String -> String -> String
binaryExpr template a b = T.unpack $ T.replace "%b" (T.pack b) $ T.replace "%a" (T.pack a) (T.pack template)

unaryExpr :: String -> String -> String
unaryExpr template a = T.unpack $ T.replace "%a" (T.pack a) (T.pack template)

unary :: (Show op, Arbitrary op, IsLuaNative op)
      => (op -> IO ()) -> Test.QuickCheck.Property
unary = flip forAllShrink shrink $ suchThat arbitrary $ not . isLuaNative

binary :: (Show op, Arbitrary op, IsLuaNative op)
       => ((op, op) -> IO ()) -> Test.QuickCheck.Property
binary = flip forAllShrink shrink $ suchThat arbitrary $ \(a, b) -> not (isLuaNative a && isLuaNative b)

binary' :: (Show op, Arbitrary op, IsLuaNative op)
        => ((op, op) -> Bool) -> ((op, op) -> IO ()) -> Test.QuickCheck.Property
binary' def = flip forAllShrink shrink $ suchThat arbitrary $ \(a, b) -> def (a, b) && not (isLuaNative a && isLuaNative b)

unary' :: (Show op, Arbitrary op, IsLuaNative op)
        => (op -> Bool) -> (op -> IO ()) -> Test.QuickCheck.Property
unary' def = flip forAllShrink shrink $ suchThat arbitrary $ \a -> def a && not (isLuaNative a)

integerLike :: forall op. IntegerLike op
            => RunLuaRun
            -> Spec op
            -> Hspec.Spec
integerLike runLua (Spec { relationalOps, binaryOps, partialOps, unaryOps }) = do
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

  describe "unary operators" $ do
    flip mapM_ unaryOps $ \(expr, MkUn ref) -> describe (unaryExpr expr "a") $ do
      it "should adhere to the reference implementation" $ properly $ unary $ \a -> do
        n <- runLua $ do
          "a" `bind` a
          return' $ unaryExpr expr "a"
        n `shouldBe` ref a
