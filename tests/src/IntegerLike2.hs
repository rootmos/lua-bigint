module IntegerLike2 where

import Test.QuickCheck
import Test.Hspec hiding ( Spec )
import qualified Test.Hspec as Hspec

import Data.ByteString.UTF8 as BSUTF8
import qualified Data.ByteString as BS

import HsLua hiding ( Integer, compare, RelationalOperator (..) )

import LuaUtils

class IsLuaNative a where
  isLuaNative :: a -> Bool

instance IsLuaNative a => IsLuaNative (NonNegative a) where
  isLuaNative = isLuaNative . getNonNegative

class (Show a, Integral a, IsLuaNative a, Arbitrary a, Peekable a, Pushable a) => IntegerLike a where

data Case = Relevant
          | Irrelevant
          | Partial String

always :: a -> Case
always _ = Relevant

relevantIfFstNotNative :: IsLuaNative a => (a, b) -> Case
relevantIfFstNotNative (a, _) | isLuaNative a = Relevant
relevantIfFstNotNative (a, _) | otherwise = Irrelevant

relevantIfNotBothLuaIntegers :: (IsLuaNative a, IsLuaNative b) => (a, b) -> Case
relevantIfNotBothLuaIntegers (a, b) =
  case (isLuaNative a, isLuaNative b) of
    (True, True) -> Irrelevant
    _ -> Relevant

data Operator a =
  forall b. MkOperator { human :: String -- "addition"
                       , ref :: a -> b
                       , isPartial :: Bool -- indicator to not try and search for non-existent partial cases
                       , function :: Maybe (String, a -> Case) -- Just "add" always
                       , method :: Maybe (String, a -> Case) -- Just "add" (relevantIfFstNotLuaIntegerOtherwiseIrrelevant) ~> "add(%a,%b)"
                       , syntax :: Maybe (String, a -> Case) -- Just "+" (relevantIfNotBothLuaIntegers)
                       }

data Spec a = MkSpec { unary :: [ Operator a ]
                     , binary :: [ Operator (a, a) ]
                     }

add :: IntegerLike a => String -> Operator (a, a)
add modname =
  MkOperator { human = "addition"
             , ref = uncurry (+)
             , isPartial = False
             , function = Just (modname ++ ".add(%a,%b)", relevantIfNotBothLuaIntegers)
             , method =  Just ("%a:add(%b)", relevantIfFstNotNative)
             }

integerLike :: forall a. IntegerLike a
            => RunLuaRun
            -> Spec a
            -> Hspec.Spec
integerLike runLua spec = do
  flip mapM_ (unary spec) $ \MkOperator { human } -> do
    describe human $ do
      return ()
