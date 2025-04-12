module BignatSpec where

import Text.Printf

--import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BSUTF8

import Test.Hspec
import Test.QuickCheck

import HsLua hiding ( Integer )

import LuaBigInt
import LuaUtils
import Huge
import Utils

runLua :: RunLuaRun
runLua = mkRun $ do
  prepare
  "M" `requireG` "bignat"

runAndPeek :: RunLuaAndPeek
runAndPeek = mkRunAndPeek runLua

evalAndPeek :: EvalLuaAndPeek
evalAndPeek = mkEvalAndPeek runAndPeek

newtype BigNat = N Integer deriving ( Show, Eq )

instance Arbitrary BigNat where
  arbitrary = N . getNonNegative <$> arbitrary

pushBigNat :: LuaError e => BigNat -> LuaE e ()
pushBigNat (N n) = ensureStackDiff 1 $ do
  OK <- dostring $ BSUTF8.fromString $ printf "return M.fromstring('%s')" (show n)
  return ()

withBigNats :: [ (Name, BigNat) ] -> RunLuaAndPeek
withBigNats ps ls = runLua $ stackNeutral $ do
  flip mapM_ ps $ \(n, b) -> stackNeutral $ do
    pushBigNat b
    setglobal n

  flip mapM_ ls $ \l -> dostring (BSUTF8.fromString l) >>= \case
    OK -> return ()
    ErrRun -> throwErrorAsException
    _ -> undefined

  a <- peek top
  pop 1
  return a

spec :: Spec
spec = do
  describe "bignat.lua" $ do
    it "should load properly" $ do
      t <- runLua $ do
        OK <- dostring "return type(M)"
        peek @String top
      t `shouldBe` "table"

    it "should set the expected max and default bases" $ do
      let e :: Int = case luaBits of { Lua32 -> 15; Lua64 -> 31 }
      evalAndPeek @Integer "M.max_base" >>= flip shouldBe (2^e)
      evalAndPeek @Integer "M.default_base" >>= flip shouldBe (2^e)

    it "build and reproduce decimal strings" $ properly $ \(NonNegative (n :: Integer)) ->
      evalAndPeek (printf "M.fromstring('%s'):tostring()" (show n)) >>= flip shouldBe (show n)
    it "build and reproduce decimal strings of huge integers" $ properly $ \(Huge { getHuge = n }) ->
      evalAndPeek (printf "M.fromstring('%s'):tostring()" (show n)) >>= flip shouldBe (show n)

    it "should produce decimal strings of integers pushed from Haskell" $ properly $ \(b@(N n)) ->
      withBigNats [ ("a", b) ] [ "return a:tostring()" ] >>= flip shouldBe (show n)
