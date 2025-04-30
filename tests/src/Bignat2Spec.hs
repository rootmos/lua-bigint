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
  "N" `bind` require "bignat"

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

instance Peekable Operand where
  safepeek idx = retrieving "operand" $ do
    n <- peekFieldRaw peekIntegral "n" idx
    as <- flip mapM [1..n] $ \i -> do
      peekIndexRaw i (\idx -> fromMaybe 0 <$> peekNilOr peekIntegral idx) idx
    o :: Integer <- peekFieldRaw peekIntegral "o" idx
    b <- peekFieldRaw peekIntegral "base" idx
    return $ OpI (Just b) $ (* b^o) $ evalInBase b as

maxBase :: Integer
maxBase = 2^(case luaBits of { Lua32 -> 15 :: Integer; Lua64 -> 31 })

genBase :: Gen (Maybe Base)
genBase = oneof [ return Nothing, Just <$> chooseInteger (2, maxBase) ]

instance Arbitrary Operand where
  arbitrary = frequency [ (60, genI)
                        , (40, genH)
                        ]
    where genI = OpI <$> genBase <*> (getNonNegative <$> arbitrary)
          genH = OpH <$> genBase <*> arbitrary

instance Pushable Operand where
  push (OpI Nothing i) = dostring' (printf "return N.fromstring('%s')" (show i))
  push (OpI (Just b) i) = dostring' (printf "return N.fromstring('%s', 10, %d)" (show i) b)
  push (OpH Nothing h) = dostring' (printf "return N.fromstring('%s')" (show $ getHuge h))
  push (OpH (Just b) h) = dostring' (printf "return N.fromstring('%s', 10, %d)" (show $ getHuge h) b)
  push _ = undefined

spec :: Spec
spec = describe "Bignat2" $ do
  --it "should work" $ properly $ \(a :: Operand) -> do
    --a `shouldBe` b

  it "should survive a push/peek roundtrip" $ properly $ \(a :: Operand) -> do
    b <- runLua $ do
      --"a" `LuaUtils.bind` (HsLua.push a)
      HsLua.push a
      peek @Operand top
    a `shouldBe` b
