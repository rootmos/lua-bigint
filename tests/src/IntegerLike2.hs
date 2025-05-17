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

relevantIfNotNative :: IsLuaNative a => a -> Case
relevantIfNotNative a | isLuaNative a = Irrelevant
relevantIfNotNative _ | otherwise = Relevant

relevantIfNative :: IsLuaNative a => a -> Case
relevantIfNative a | isLuaNative a = Relevant
relevantIfNative _ | otherwise = Irrelevant

relevantIfFstNotNative :: IsLuaNative a => (a, b) -> Case
relevantIfFstNotNative (a, _) | isLuaNative a = Irrelevant
relevantIfFstNotNative (_, _) | otherwise = Relevant

relevantIfNotBothLuaIntegers :: (IsLuaNative a, IsLuaNative b) => (a, b) -> Case
relevantIfNotBothLuaIntegers (a, b) =
  case (isLuaNative a, isLuaNative b) of
    (True, True) -> Irrelevant
    _ -> Relevant

 -- TODO split into MkBin, MkUn, MkDual so b can be Peekable and Pushable accordingly
data Operator a = forall b. (Show b, Eq b, Peekable b, Pushable b)
  => MkOperator { human :: String
                , ref :: a -> b
                , isDual :: Bool -- indicator to test the dual of the reference implementation
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
             , isDual = False
             , isPartial = False
             , syntax = Just ("%a + %b", relevantIfNotBothLuaIntegers)
             , function = Just (modname ++ ".add(%a,%b)", relevantIfNotBothLuaIntegers)
             , method =  Just ("%a:add(%b)", relevantIfFstNotNative)
             }

tostring :: IntegerLike a => String -> Operator a
tostring modname =
  MkOperator { human = "convert to decimal representation"
             , ref = show . toInteger
             , isDual = False
             , isPartial = False
             , syntax = Just ("tostring(%a)", always)
             , function = Just (modname ++ ".tostring(%a)", relevantIfNotNative)
             , method =  Just ("%a:tostring()", relevantIfNotNative)
             }

fromstring :: IntegerLike a => String -> Operator a
fromstring modname =
  MkOperator { human = "convert from decimal representation"
             , ref = show . toInteger
             , isDual = True
             , isPartial = False
             , syntax = Nothing
             , function = Just (modname ++ ".fromstring(%b)", always)
             , method =  Nothing
             }

newtype SafeToInteger = MkSafeToInteger (Maybe Integer) deriving ( Show, Eq )

instance Peekable SafeToInteger where
  safepeek idx = cleanup $ liftLua $ isnil idx >>= \case
    True -> return $ MkSafeToInteger Nothing
    False -> MkSafeToInteger . Just <$> peek idx

instance Pushable SafeToInteger where
  push = undefined

safeToInteger :: Integral a => a -> SafeToInteger
safeToInteger a = MkSafeToInteger $
  if abs a <= maxint then Just (toInteger a) else Nothing

tointeger :: IntegerLike a => String -> Operator a
tointeger modname =
  MkOperator { human = "safe conversion to native integer"
             , ref = safeToInteger
             , isDual = False
             , isPartial = False
             , syntax = Nothing
             , function = Just (modname ++ ".tointeger(%a)", relevantIfNotNative)
             , method =  Just ("%a:tointeger()", relevantIfNotNative)
             }

frominteger :: IntegerLike a => String -> Operator a
frominteger modname =
  MkOperator { human = "convert from native integers"
             , ref = toInteger
             , isDual = True
             , isPartial = False
             , syntax = Nothing
             , function = Just (modname ++ ".frominteger(%b)", relevantIfNative)
             , method =  Nothing
             }

mkProp :: (Show c, Arbitrary c)
       => (c -> Case) -> (c -> IO ()) -> Test.QuickCheck.Property
mkProp study = flip forAllShrink shrink $ suchThat arbitrary ((== Relevant) . study)

mkExpr :: String -> [ (String, String) ] -> String
mkExpr template rs = T.unpack $ r rs $ T.pack template
  where r [] s = s
        r ((from, to):rs) s = r rs $ T.replace (T.pack from) (T.pack to) s

integerLike :: forall a. IntegerLike a
            => RunLuaRun
            -> Spec a
            -> Hspec.Spec
integerLike runLua spec = do
  flip mapM_ (binary spec) $ \MkOperator { human, ref, syntax, function, method } -> do
    describe human $ do
      forM_ (catMaybes [ syntax, function, method ]) $ \(expr, study) -> do
        let expr' = mkExpr expr [ ("%a", "a"), ("%b", "b") ]
        it expr' $ properly $ mkProp study $ \(a, b) -> do
          s <- runLua $ do
            "a" `bind` a
            "b" `bind` b
            return' expr'
          s `shouldBe` ref (a, b)

        let expr' = mkExpr expr [ ("%a", "a"), ("%b", "b") ]
        it (expr' ++ " (when a == b)") $ properly $ mkProp (study . \a -> (a, a)) $ \a -> do
          s <- runLua $ do
            "a" `bind` a
            "b" `bind` a
            return' expr'
          s `shouldBe` ref (a, a)

        let expr' = mkExpr expr [ ("%a", "a"), ("%b", "a") ]
        it expr' $ properly $ mkProp (study . \a -> (a, a)) $ \a -> do
          s <- runLua $ do
            "a" `bind` a
            return' expr'
          s `shouldBe` ref (a, a)

  flip mapM_ (unary spec) $ \MkOperator { human, ref, isDual, syntax, function, method } -> do
    describe human $ case isDual of
      False -> do
        forM_ (catMaybes [ syntax, function, method ]) $ \(expr, study) -> do
          let expr' = mkExpr expr [ ("%a", "a") ]
          it expr' $ properly $ mkProp study $ \a -> do
            s <- runLua $ do
              "a" `bind` a
              return' expr'
            s `shouldBe` ref a
      True -> do
        forM_ (catMaybes [ syntax, function, method ]) $ \(expr, study) -> do
          let expr' = mkExpr expr [ ("%b", "b") ]
          it expr' $ properly $ mkProp study $ \a -> do
            s <- runLua $ do
              "b" `bind` (ref a)
              return' expr'
            s `shouldBe` a
