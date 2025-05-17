module IntegerLike2 where

import Control.Monad ( forM_ )
import Data.Maybe ( catMaybes )
import qualified Data.Text as T

--import Data.ByteString.UTF8 as BSUTF8
--import qualified Data.ByteString as BS

import Test.QuickCheck hiding ( function )
import Test.Hspec hiding ( Spec )
import qualified Test.Hspec as Hspec

import HsLua hiding ( Integer, compare, RelationalOperator (..), ref, method )

import LuaUtils
import Utils

class IsLuaNative a where
  isLuaNative :: a -> Bool

instance IsLuaNative a => IsLuaNative (NonNegative a) where
  isLuaNative = isLuaNative . getNonNegative

class (Show a, Integral a, IsLuaNative a, Arbitrary a, Peekable a, Pushable a) => IntegerLike a where

data Case = Relevant
          | Irrelevant
          | Partial String
          deriving ( Show, Eq )

always :: a -> Case
always _ = Relevant

relevantIfFstNotNative :: IsLuaNative a => (a, b) -> Case
relevantIfFstNotNative (a, _) | isLuaNative a = Irrelevant
relevantIfFstNotNative (_, _) | otherwise = Relevant

relevantIfNotBothLuaIntegers :: (IsLuaNative a, IsLuaNative b) => (a, b) -> Case
relevantIfNotBothLuaIntegers (a, b) =
  case (isLuaNative a, isLuaNative b) of
    (True, True) -> Irrelevant
    _ -> Relevant

data Operator a = forall b. (Show b, Eq b, Peekable b)
  => MkOperator { human :: String
                , ref :: a -> b
                , isPartial :: Bool -- indicator to not try and search for non-existent partial cases
                , syntax :: Maybe (String, a -> Case)
                , function :: Maybe (String, a -> Case)
                , method :: Maybe (String, a -> Case)
                } -- laws?

data Spec a = MkSpec { unary :: [ Operator a ]
                     , binary :: [ Operator (a, a) ]
                     }

add :: IntegerLike a => String -> Operator (a, a)
add modname =
  MkOperator { human = "addition"
             , ref = uncurry (+)
             , isPartial = False
             , syntax = Just ("%a + %b", relevantIfNotBothLuaIntegers)
             , function = Just (modname ++ ".add(%a,%b)", relevantIfNotBothLuaIntegers)
             , method =  Just ("%a:add(%b)", relevantIfFstNotNative)
             }

mkProp :: (Show c, Arbitrary c)
       => (c -> Case) -> (c -> IO ()) -> Test.QuickCheck.Property
mkProp study = flip forAllShrink shrink $ suchThat arbitrary ((== Relevant) . study)

binaryExpr :: String -> String -> String -> String
binaryExpr template a b = T.unpack $ T.replace "%b" (T.pack b) $ T.replace "%a" (T.pack a) (T.pack template)

integerLike :: forall a. IntegerLike a
            => RunLuaRun
            -> Spec a
            -> Hspec.Spec
integerLike runLua spec = do
  flip mapM_ (binary spec) $ \MkOperator { human, ref, syntax, function, method } -> do
    describe human $ do
      forM_ (catMaybes [ syntax, function, method ]) $ \(expr, study) -> do
        let expr' = binaryExpr expr "a" "b"
        describe expr' $ do
          it "should adhere to the reference implementation" $ properly $ mkProp study $ \(a, b) -> do
            s <- runLua $ do
              "a" `bind` a
              "b" `bind` b
              return' expr'
            s `shouldBe` ref (a, b)
