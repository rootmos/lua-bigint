module Bignat2Spec where

import Control.Monad ( when )
import Data.Maybe ( fromMaybe )
import System.IO.Unsafe ( unsafePerformIO )
import System.Random ( randomIO )
import Text.Printf

import Test.Hspec
import Test.QuickCheck

import HsLua hiding ( Integer, compare )
import HsLua.Marshalling.Peekers

import Huge
import LuaUtils
import Utils
import LuaBigInt

runLua :: RunLuaRun
runLua = flip (.) stackNeutral $ mkRun $ do
  prepare
  "N" `requireG` "bignat"

type Base = Integer

data Operand = OpI (Maybe Base) Integer
             | OpL LuaInt
             | OpH (Maybe Base) Huge
             | OpO Base Int Integer

             deriving ( Show )
--instance Show Operand where
  --show = show . operandToInteger

operandToInteger :: Operand -> Integer
operandToInteger (OpI _ i) = i
operandToInteger (OpH _ h) = getHuge h
operandToInteger (OpO b o i) = (b^o) * i
operandToInteger _ = undefined

instance Eq Operand where
  a == b = operandToInteger a == operandToInteger b

instance Ord Operand where
  compare a b = compare (operandToInteger a) (operandToInteger b)

instance Num Operand where
  a + b = OpI Nothing $ operandToInteger a + operandToInteger b
  a * b = OpI Nothing $ operandToInteger a * operandToInteger b
  abs a = OpI Nothing $ abs $ operandToInteger a
  signum a = OpI Nothing $ signum $ operandToInteger a
  negate = undefined
  fromInteger i = OpI Nothing i

instance Peekable Operand where
  safepeek idx = retrieving "operand" $ do
    n <- peekFieldRaw peekIntegral "n" idx
    as <- flip mapM [1..n] $ \i -> do
      peekIndexRaw i (\idx -> fromMaybe 0 <$> peekNilOr peekIntegral idx) idx
    o :: Integer <- peekFieldRaw peekIntegral "o" idx
    b <- peekFieldRaw peekIntegral "base" idx
    return $ OpI (Just b) $ (* b^o) $ evalInBase b as

instance Pushable Operand where
  push (OpI Nothing i) = dostring' (printf "return N.fromstring('%s')" (show i))
  push (OpI (Just b) i) = dostring' (printf "return N.fromstring('%s', 10, %d)" (show i) b)
  push (OpH Nothing h) = dostring' (printf "return N.fromstring('%s')" (show $ getHuge h))
  push (OpH (Just b) h) = dostring' (printf "return N.fromstring('%s', 10, %d)" (show $ getHuge h) b)
  push (OpO b o i) = ensureStackDiff 1 $ do
    dostring' "return N.make"

    let ds = digitsInBase b i
        n = length ds

    createtable n 3
    t <- gettop

    pushinteger $ fromIntegral b
    setfield t "base"

    flip mapM_ (zip ds [1..]) $ \(a, k) -> stackNeutral $ do
      pushinteger (fromIntegral a)
      rawseti t k

    when (o /= 0 || unsafePerformIO randomIO) $ stackNeutral $ do
      pushinteger $ fromIntegral o
      setfield t "o"

    when (unsafePerformIO randomIO) $ stackNeutral $ do
      pushinteger $ fromIntegral n
      setfield t "n"

    call 1 1
  push _ = undefined

instance Arbitrary Operand where
  arbitrary = frequency [ (50, genI)
                        , (30, genH)
                        , (20, genO)
                        ]
    where genBase = chooseInteger (2, maxBase)
          genBaseM = oneof [ return Nothing, Just <$> genBase ]
          genI = OpI <$> genBaseM <*> (getNonNegative <$> arbitrary)
          genH = OpH <$> genBaseM <*> arbitrary
          genO = do
            b <- genBase
            o <- getNonNegative <$> arbitrary
            i <- operandToInteger <$> oneof [ genI, genH ]
            return $ OpO b o i

  shrink (OpI mb i) = OpI mb <$> shrink i
  shrink (OpH mb h) = OpH mb <$> shrink h
  shrink (OpO b o i) = do
    o' <- shrink o
    i' <- shrink i
    b' <- filter (>= 2) $ shrink b
    return $ OpO b' o' i'
  shrink _ = undefined

spec :: Spec
spec = do
  it "should have the expected max_base" $ do
    b <- runLua $ return' "N.max_base"
    b `shouldBe` maxBase

  it "should survive a push and peek roundtrip" $ properly $ \(a :: Operand) -> do
    b <- runLua $ push a >> peek'
    b `shouldBe` a

  it "should add properly" $ properly $ \(a :: Operand, b :: Operand) -> do
    s <- runLua $ do
      "a" `bind` a
      "b" `bind` b
      return' "a + b"
    s `shouldBe` a + b

  it "should multiply properly" $ properly $ \(a :: Operand, b :: Operand) -> do
    s <- runLua $ do
      "a" `bind` a
      "b" `bind` b
      return' "a * b"
    s `shouldBe` a * b

  describe "relational operators" $ do
    let operators :: [ (String, Bool, (forall a. (Eq a, Ord a) => a -> a -> Bool)) ]
        operators = [ ("==", True, (==))
                    , ("~=", False, (/=))
                    , ("<", False, (<))
                    , ("<=", True, (<=))
                    , (">", False, (>))
                    , (">=", True, (>=))
                    ]
    flip mapM_ operators $ \(oplua, refl, op) -> do
      it (printf "(%s) should %s be reflexive (by reference)" oplua (be refl)) $ properly $ \(a :: Operand) -> do
        s <- runLua $ do
          "a" `bind` a
          return' $ printf "a %s a" oplua
        s `shouldBe` op a a
      it (printf "(%s) should %s be reflexive (by value)" oplua (be refl)) $ properly $ \(a :: Operand) -> do
        s <- runLua $ do
          "a" `bind` a
          "b" `bind` a
          return' $ printf "a %s b" oplua
        s `shouldBe` op a a
