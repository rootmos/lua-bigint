module Bignat2Spec where

import Data.Maybe ( fromMaybe )
import Text.Printf

import Test.Hspec
import Test.QuickCheck

import HsLua hiding ( Integer )
import HsLua.Marshalling.Peekers

import Huge
import LuaUtils
import Utils
import LuaBigInt

runLua :: RunLuaRun
runLua = mkRun $ do
  prepare
  "N" `requireG` "bignat"

type Base = Integer

data Operand = OpI (Maybe Base) Integer
             | OpL LuaInt
             | OpH (Maybe Base) Huge
             | OpO Base Int Integer
             deriving ( Show )

operandToInteger :: Operand -> Integer
operandToInteger (OpI _ i) = i
operandToInteger (OpH _ h) = getHuge h
operandToInteger _ = undefined

instance Eq Operand where
  a == b = operandToInteger a == operandToInteger b

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
  push _ = undefined

genBase :: Gen (Maybe Base)
genBase = oneof [ return Nothing, Just <$> chooseInteger (2, maxBase) ]

instance Arbitrary Operand where
  arbitrary = frequency [ (60, genI)
                        , (40, genH)
                        ]
    where genI = OpI <$> genBase <*> (getNonNegative <$> arbitrary)
          genH = OpH <$> genBase <*> arbitrary

  shrink (OpI mb i) = OpI mb <$> shrink i
  shrink (OpH mb h) = OpH mb <$> shrink h
  shrink _ = undefined

spec :: Spec
spec = describe "Bignat2" $ do
  it "should have the expected max_base" $ do
    b <- runLua $ dostring' "return N.max_base" >> peek top
    b `shouldBe` maxBase

  it "should survive a push/peek roundtrip" $ properly $ \(a :: Operand) -> do
    b <- runLua $ do
      HsLua.push a
      peek top
    a `shouldBe` b

  it "should add properly" $ properly $ \(a :: Operand, b :: Operand) -> do
    s <- runLua $ do
      "a" `bind` a
      "b" `bind` b
      dostring' "return a + b"
      peek top
    s `shouldBe` a + b

  it "should multiply properly" $ properly $ \(a :: Operand, b :: Operand) -> do
    s <- runLua $ do
      "a" `bind` a
      "b" `bind` b
      dostring' "return a * b"
      peek top
    s `shouldBe` a * b
