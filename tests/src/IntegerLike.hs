module IntegerLike where

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

import Lib
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

relevantIfNeitherIsNative :: (IsLuaNative a, IsLuaNative b) => (a, b) -> Case
relevantIfNeitherIsNative (a, b) =
  case (isLuaNative a, isLuaNative b) of
    (False, False) -> Relevant
    (_, _) -> Irrelevant

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

quot :: IntegerLike a => String -> Operator (a, a)
quot modname =
  MkOperator { human = "integer division truncated towards zero"
             , ref = uncurry Prelude.quot
             , isDual = False
             , isPartial = True
             , syntax = Just ("%a // %b", relevantIfNotBothLuaIntegers <> divByZero)
             , function = Just (modname ++ ".quot(%a,%b)", relevantIfNotBothLuaIntegers <> divByZero)
             , method =  Just ("%a:quot(%b)", relevantIfFstNotNative <> divByZero)
             }

rem :: IntegerLike a => String -> Operator (a, a)
rem modname =
  MkOperator { human = "remainder after integer division truncated towards zero"
             , ref = uncurry Prelude.rem
             , isDual = False
             , isPartial = True
             , syntax = Just ("%a % %b", relevantIfNotBothLuaIntegers <> divByZero)
             , function = Just (modname ++ ".rem(%a,%b)", relevantIfNotBothLuaIntegers <> divByZero)
             , method =  Just ("%a:rem(%b)", relevantIfFstNotNative <> divByZero)
             }

quotrem :: IntegerLike a => String -> Operator (a, a)
quotrem modname =
  MkOperator { human = "quotrem function"
             , ref = uncurry quotRem
             , isDual = False
             , isPartial = True
             , syntax = Nothing
             , function = Just (printf "{%s.quotrem(%%a,%%b)}" modname, relevantIfNotBothLuaIntegers <> divByZero)
             , method =  Just ("{%a:quotrem(%b)}", relevantIfFstNotNative <> divByZero)
             }

div :: IntegerLike a => String -> Operator (a, a)
div modname =
  MkOperator { human = "integer division truncated towards negative infinity"
             , ref = uncurry Prelude.div
             , isDual = False
             , isPartial = True
             , syntax = Nothing
             , function = Just (modname ++ ".div(%a,%b)", relevantIfNotBothLuaIntegers <> divByZero)
             , method =  Just ("%a:div(%b)", relevantIfFstNotNative <> divByZero)
             }

mod :: IntegerLike a => String -> Operator (a, a)
mod modname =
  MkOperator { human = "remainder after integer division truncated towards negative infinity"
             , ref = uncurry Prelude.mod
             , isDual = False
             , isPartial = True
             , syntax = Nothing
             , function = Just (modname ++ ".mod(%a,%b)", relevantIfNotBothLuaIntegers <> divByZero)
             , method =  Just ("%a:mod(%b)", relevantIfFstNotNative <> divByZero)
             }

divmod :: IntegerLike a => String -> Operator (a, a)
divmod modname =
  MkOperator { human = "divmod function"
             , ref = uncurry divMod
             , isDual = False
             , isPartial = True
             , syntax = Nothing
             , function = Just (printf "{%s.divmod(%%a,%%b)}" modname, relevantIfNotBothLuaIntegers <> divByZero)
             , method =  Just ("{%a:divmod(%b)}", relevantIfFstNotNative <> divByZero)
             }

abs :: forall a. IntegerLike a => String -> Operator a
abs modname =
  MkOperator { human = "absolute value"
             , ref = Prelude.abs @a
             , isDual = False
             , isPartial = False
             , syntax = Nothing
             , function = Just (modname ++ ".abs(%a)", relevantIfNotNative)
             , method =  Just ("%a:abs()", relevantIfNotNative)
             }

sign :: IntegerLike a => String -> Operator a
sign modname =
  MkOperator { human = "signum"
             , ref = Prelude.signum . toInteger
             , isDual = False
             , isPartial = False
             , syntax = Nothing
             , function = Just (modname ++ ".sign(%a)", relevantIfNotNative)
             , method =  Just ("%a:sign()", relevantIfNotNative)
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
relationalOperators modname = fmap mk' [ ("equality", (==), "==", "eq")
                                       , ("not equal", (/=), "~=", "neq")
                                       ] ++
                              fmap mk [ ("less than", (<), "<", "lt")
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
        mk' (human, ref, syntax :: String, method :: String) =
               MkOperator { human = human
                          , ref = uncurry ref
                          , isDual = False
                          , isPartial = False
                          , syntax = Just (printf "%%a %s %%b" syntax, relevantIfNeitherIsNative)
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

tohex :: IntegerLike a => String -> Operator a
tohex modname =
  MkOperator { human = "convert to hexadecimal representation"
             , ref = toHex . toInteger
             , isDual = False
             , isPartial = False
             , syntax = Nothing
             , function = Just (modname ++ ".tohex(%a)", relevantIfNotNative)
             , method =  Just ("%a:tohex()", relevantIfNotNative)
             }

fromhex :: IntegerLike a => String -> Operator a
fromhex modname =
  MkOperator { human = "convert from hexadecimal representation"
             , ref = toHex . toInteger
             , isDual = True
             , isPartial = False
             , syntax = Nothing
             , function = Just (modname ++ ".fromhex(%b)", always)
             , method =  Nothing
             }

tobigendian :: IntegerLike a => String -> Operator a
tobigendian modname =
  MkOperator { human = "convert to big-endian bytes"
             , ref = toBeBytes . toInteger
             , isDual = False
             , isPartial = False
             , syntax = Nothing
             , function = Just (modname ++ ".tobigendian(%a)", relevantIfNotNative)
             , method =  Just ("%a:tobigendian()", relevantIfNotNative)
             }

frombigendian :: IntegerLike a => String -> Operator a
frombigendian modname =
  MkOperator { human = "convert from big-endian bytes"
             , ref = toBeBytes . toInteger
             , isDual = True
             , isPartial = False
             , syntax = Nothing
             , function = Just (modname ++ ".frombigendian(%b)", always)
             , method =  Nothing
             }

tolittleendian :: IntegerLike a => String -> Operator a
tolittleendian modname =
  MkOperator { human = "convert to little-endian bytes"
             , ref = toLeBytes . toInteger
             , isDual = False
             , isPartial = False
             , syntax = Nothing
             , function = Just (modname ++ ".tolittleendian(%a)", relevantIfNotNative)
             , method =  Just ("%a:tolittleendian()", relevantIfNotNative)
             }

fromlittleendian :: IntegerLike a => String -> Operator a
fromlittleendian modname =
  MkOperator { human = "convert from little-endian bytes"
             , ref = toLeBytes . toInteger
             , isDual = True
             , isPartial = False
             , syntax = Nothing
             , function = Just (modname ++ ".fromlittleendian(%b)", always)
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
  if minint <= a && a <= maxint then Just (toInteger a) else Nothing

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

tobase :: IntegerLike a => String -> Operator (a, Base)
tobase modname =
  MkOperator { human = "convert to digits in base"
             , ref = \(a, MkBase b) -> let ds = digitsInBase b (Prelude.abs $ toInteger a) in (ds, length ds)
             , isDual = False
             , isPartial = False
             , syntax = Nothing
             , function = Just (printf "{%s.tobase(%%a,%%b):digits()}" modname, relevantIfFstNotNative)
             , method = Just ("{%a:tobase(%b):digits()}", relevantIfFstNotNative)
             }

mkProp :: (Show c, Arbitrary c)
       => (c -> Case) -> (c -> IO ()) -> Test.QuickCheck.Property
mkProp study = flip forAllShrink (filter p <$> shrink) $ suchThat arbitrary p
  where p = ((== Relevant) . study)

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

runSpec :: forall a. IntegerLike a
        => RunLuaRun
        -> Spec a
        -> Hspec.Spec
runSpec runLua spec = do
  forM_ (binary spec) (runaa runLua)
  forM_ (unary spec) (run1 runLua)

run1 :: (Eq a, Show a, Peekable a, Pushable a, Arbitrary a)
     => RunLuaRun
     -> Operator a
     -> Hspec.Spec
run1 runLua (MkOperator { human, ref, isDual, isPartial, syntax, function, method }) = do
  describe human $ do
    case isDual of
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

    when isPartial $ do
      forM_ (catMaybes [ syntax, function, method ]) $ \(expr, study) -> do
        let expr' = mkExpr expr [ ("%a", "a") ]
        it (printf "%s should raise the expected error when not defined" expr') $ properly $ mkPropPartial study $ \(a, msg) -> do
          Just (Exception msg') <- runLua $ do
            "a" `bind` a
            expectError' expr'
          msg' `shouldEndWith` msg

run2 :: (Pushable a, Show a, Arbitrary a, Pushable b, Show b, Arbitrary b)
     => RunLuaRun
     -> Operator (a, b)
     -> Hspec.Spec
run2 runLua (MkOperator { human, ref, isPartial, syntax, function, method }) = do
  describe human $ do
    forM_ (catMaybes [ syntax, function, method ]) $ \(expr, study) -> do
      let expr' = mkExpr expr [ ("%a", "a"), ("%b", "b") ]
      it expr' $ properly $ mkProp study $ \(a, b) -> do
        s <- runLua $ do
          "a" `bind` a
          "b" `bind` b
          return' expr'
        s `shouldBe` ref (a, b)

    when isPartial $ do
      forM_ (catMaybes [ syntax, function, method ]) $ \(expr, study) -> do
        let expr' = mkExpr expr [ ("%a", "a"), ("%b", "b") ]
        it (printf "%s should raise the expected error when not defined" expr') $ properly $ mkPropPartial study $ \((a, b), msg) -> do
          Just (Exception msg') <- runLua $ do
            "a" `bind` a
            "b" `bind` b
            expectError' expr'
          msg' `shouldEndWith` msg

runaa :: (Pushable a, Show a, Arbitrary a)
      => RunLuaRun
      -> Operator (a, a)
      -> Hspec.Spec
runaa runLua (MkOperator { human, ref, isPartial, syntax, function, method }) = do
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
