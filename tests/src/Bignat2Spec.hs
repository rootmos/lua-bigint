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
    return $ OpI (Just b) . (* b^o) $ evalInBase b as

maxBase :: Integer
maxBase = 2^(case luaBits of { Lua32 -> 15 :: Integer; Lua64 -> 31 })

genBase :: Gen (Maybe Base)
genBase = oneof [ return Nothing, Just <$> chooseInteger (2, maxBase) ]

instance Arbitrary Operand where
  arbitrary = do
    b <- genBase
    OpI b . getNonNegative <$> arbitrary

instance Pushable Operand where
  push (OpI Nothing i) = dostring' (printf "return N.fromstring('%s')" (show i))
  push (OpI (Just b) i) = dostring' (printf "return N.fromstring('%s', 10, %d)" (show i) b)
  push _ = undefined

-- with some invariants:
-- push:ing defaults Nothing to M.default_base
-- peek:ing always sets base, and never produce H3?
-- comparison/conversion doesn't use the maybe base in I3 and H3

-- gen coin-flips: I3,H3 Nothing or random [2,max_base]
-- gen coin-flips: Z3 max_base or random [2,max_base]

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
