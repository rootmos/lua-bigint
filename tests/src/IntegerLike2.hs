module IntegerLike2 where

import Prelude

import Control.Monad ( forM_, when )
import Data.Maybe ( catMaybes )
import qualified Data.Text as T
import Text.Printf

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

divByZero :: Integral b => (a, b) -> Case
divByZero (_, b) | b == 0 = Partial "attempt to divide by zero"
divByZero (_, _) | otherwise = Relevant

instance Semigroup Case where
  Relevant <> Relevant = Relevant
  Irrelevant <> _ = Irrelevant
  _ <> Irrelevant = Irrelevant
  Partial msg <> Relevant = Partial msg
  Relevant <> Partial msg = Partial msg
  Partial msg <> Partial _ = Partial msg

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

sub :: IntegerLike a => String -> Operator (a, a)
sub modname =
  MkOperator { human = "subtraction"
             , ref = uncurry (-)
             , isDual = False
             , isPartial = False
             , syntax = Just ("%a - %b", relevantIfNotBothLuaIntegers)
             , function = Just (modname ++ ".sub(%a,%b)", relevantIfNotBothLuaIntegers)
             , method =  Just ("%a:sub(%b)", relevantIfFstNotNative)
             }

neg :: IntegerLike a => String -> Operator a
neg modname =
  MkOperator { human = "negation"
             , ref = negate
             , isDual = False
             , isPartial = False
             , syntax = Just ("-%a", relevantIfNotNative)
             , function = Just (modname ++ ".neg(%a)", relevantIfNotNative)
             , method =  Just ("%a:neg()", relevantIfNotNative)
             }

mul :: IntegerLike a => String -> Operator (a, a)
mul modname =
  MkOperator { human = "multiplication"
             , ref = uncurry (*)
             , isDual = False
             , isPartial = False
             , syntax = Just ("%a * %b", relevantIfNotBothLuaIntegers)
             , function = Just (modname ++ ".mul(%a,%b)", relevantIfNotBothLuaIntegers)
             , method =  Just ("%a:mul(%b)", relevantIfFstNotNative)
             }

div :: IntegerLike a => String -> Operator (a, a)
div modname =
  MkOperator { human = "floor division"
             , ref = uncurry quot
             , isDual = False
             , isPartial = True
             , syntax = Just ("%a // %b", relevantIfNotBothLuaIntegers <> divByZero)
             , function = Just (modname ++ ".div(%a,%b)", relevantIfNotBothLuaIntegers <> divByZero)
             , method =  Just ("%a:div(%b)", relevantIfFstNotNative <> divByZero)
             }

mod :: IntegerLike a => String -> Operator (a, a)
mod modname =
  MkOperator { human = "modulo"
             , ref = uncurry rem
             , isDual = False
             , isPartial = True
             , syntax = Just ("%a % %b", relevantIfNotBothLuaIntegers <> divByZero)
             , function = Just (modname ++ ".mod(%a,%b)", relevantIfNotBothLuaIntegers <> divByZero)
             , method =  Just ("%a:mod(%b)", relevantIfFstNotNative <> divByZero)
             }

divrem :: IntegerLike a => String -> Operator (a, a)
divrem modname =
  MkOperator { human = "divrem function"
             , ref = uncurry quotRem
             , isDual = False
             , isPartial = True
             , syntax = Nothing
             , function = Just (printf "{%s.divrem(%%a,%%b)}" modname, relevantIfNotBothLuaIntegers <> divByZero)
             , method =  Just ("{%a:divrem(%b)}", relevantIfFstNotNative <> divByZero)
             }

compare :: IntegerLike a => String -> Operator (a, a)
compare modname =
  MkOperator { human = "comparison"
             , ref = \(a, b) -> case Prelude.compare a b of
                                  LT -> -1 :: Int
                                  EQ -> 0
                                  GT -> 1
             , isDual = False
             , isPartial = False
             , syntax = Nothing
             , function = Just (modname ++ ".compare(%a,%b)", relevantIfNotBothLuaIntegers)
             , method =  Just ("%a:compare(%b)", relevantIfFstNotNative)
             }

relationalOperators :: IntegerLike a => String -> [ Operator (a, a) ]
relationalOperators modname = fmap mk [ ("equality", (==), "==", "eq")
                                      , ("not equal", (/=), "~=", "neq")
                                      , ("less than", (<), "<", "lt")
                                      , ("less than or equal", (<=), "<=", "le")
                                      , ("greater than", (>), ">", "gt")
                                      , ("greater than or equal", (>=), ">=", "ge")
                                      ]
  where mk (human, ref, syntax :: String, method :: String) =
               MkOperator { human = human
                          , ref = uncurry ref
                          , isDual = False
                          , isPartial = False
                          , syntax = Just (printf "%%a %s %%b" syntax, relevantIfNotBothLuaIntegers)
                          , function = Just (printf "%s.%s(%%a, %%b)" modname method, relevantIfNotBothLuaIntegers)
                          , method =  Just (printf "%%a:%s(%%b)" method, relevantIfFstNotNative)
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

mkPropPartial :: (Show c, Arbitrary c)
              => (c -> Case) -> ((c, String) -> IO ()) -> Test.QuickCheck.Property
mkPropPartial study = flip forAllShrink shrink $ suchThatMap arbitrary $ \a ->
  case study a of
    Partial msg -> Just (a, msg)
    _ -> Nothing

mkExpr :: String -> [ (String, String) ] -> String
mkExpr template rs = T.unpack $ r rs $ T.pack template
  where r [] s = s
        r ((from, to):rs) s = r rs $ T.replace (T.pack from) (T.pack to) s

integerLike :: forall a. IntegerLike a
            => RunLuaRun
            -> Spec a
            -> Hspec.Spec
integerLike runLua spec = do
  flip mapM_ (binary spec) $ \MkOperator { human, ref, isPartial, syntax, function, method } -> do
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
        it (expr' ++ " (when a == b by value)") $ properly $ mkProp (study . \a -> (a, a)) $ \a -> do
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

      when isPartial $ do
        forM_ (catMaybes [ syntax, function, method ]) $ \(expr, study) -> do
          let expr' = mkExpr expr [ ("%a", "a"), ("%b", "b") ]
          it (printf "%s should raise the expected error when not defined" expr') $ properly $ mkPropPartial study $ \((a, b), msg) -> do
            Just (Exception msg') <- runLua $ do
              "a" `bind` a
              "b" `bind` b
              expectError' expr'
            msg' `shouldEndWith` msg

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
