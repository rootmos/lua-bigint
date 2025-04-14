module BignatSpec where

import Text.Printf

--import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BSUTF8

import Test.Hspec
import Test.QuickCheck

import HsLua hiding ( Integer, compare, RelationalOperator (..) )

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

newtype BigNat = N Integer deriving ( Show, Eq, Ord, Num )

instance Arbitrary BigNat where
  arbitrary = N . getHuge <$> arbitrary

pushBigNat :: LuaError e => BigNat -> LuaE e ()
pushBigNat (N n) = ensureStackDiff 1 $ do
  OK <- dostring $ BSUTF8.fromString $ printf "return M.fromstring('%s')" (show n)
  return ()

instance Peekable BigNat where
  safepeek idx = retrieving "BigNat" $ cleanup $ do
    liftLua $ do
      i <- absindex idx
      TypeFunction <- getfield i "tostring"
      pushvalue i
      call 1 1
    N . read <$> peekString top

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

    do
      let e :: Int = case luaBits of { Lua32 -> 15; Lua64 -> 31 }
      it (printf "should set the expected max and default bases (2^e = 2^%d = %x)" e ((2 :: Int)^e)) $ do
        evalAndPeek @Integer "M.max_base" >>= flip shouldBe (2^e)
        evalAndPeek @Integer "M.default_base" >>= flip shouldBe (2^e)

    describe "decimal representation" $ do
      it "should build and reproduce decimal strings" $ properly $ \(NonNegative (n :: Integer)) ->
        evalAndPeek (printf "M.fromstring('%s'):tostring()" (show n)) >>= flip shouldBe (show n)
      it "should build and reproduce decimal strings of huge integers" $ properly $ \(Huge { getHuge = n }) ->
        evalAndPeek (printf "M.fromstring('%s'):tostring()" (show n)) >>= flip shouldBe (show n)
      it "should produce decimal strings of integers pushed from Haskell" $ properly $ \(a@(N n)) ->
        withBigNats [ ("a", a) ] [ "return a:tostring()" ] >>= flip shouldBe (show n)

    describe "BigNat" $ do
      it "should push value of the expected type" $ properly $ \a ->
        withBigNats [ ("a", a) ] [ "return M.is_bignat(a)" ] >>= flip shouldBe True
      it "should peek the pushed BigNat" $ properly $ \a ->
        withBigNats [ ("a", a) ] [ "return a" ] >>= flip shouldBe a

    describe "addition" $ do
      it "should add integers" $ properly $ \(NonNegative a, NonNegative b) ->
        withBigNats [ ("a", N a), ("b", N b) ] [ "return M.add(a, b)" ] >>= flip shouldBe (N $ a + b)
      it "should add huge integers" $ properly $ \(a, b) ->
        withBigNats [ ("a", a), ("b", b) ] [ "return M.add(a, b)" ] >>= flip shouldBe (a + b)
      it "should __add integers" $ properly $ \(a, b) ->
        withBigNats [ ("a", a), ("b", b) ] [ "return a + b" ] >>= flip shouldBe (a + b)

    describe "multiplication" $ do
      it "should multiply integers" $ properly $ \(NonNegative a, NonNegative b) ->
        withBigNats [ ("a", N a), ("b", N b) ] [ "return M.mul(a, b)" ] >>= flip shouldBe (N $ a * b)
      it "should multiply huge integers" $ properly $ \(a, b) ->
        withBigNats [ ("a", a), ("b", b) ] [ "return M.mul(a, b)" ] >>= flip shouldBe (a * b)
      it "should __mul integers" $ properly $ \(a, b) ->
        withBigNats [ ("a", a), ("b", b) ] [ "return a * b" ] >>= flip shouldBe (a * b)

      describe "examples" $ do
        let example a b prod = context ("a := " ++ show a) . context ("b := " ++ show b) . context ("prod := " ++ show prod) $ do
             it (printf "should compute a * b correctly") $ do
               withBigNats [ ("a", a), ("b", b) ] [ "return a * b" ] >>= flip shouldBe (N prod)
        example
          3205602854067328
          249372854764725493398502463869928986413831547223573540317414832694304662339311947051439428212576348058279892962899721479168
          799390334960721315730397095527010617512940211652393567331857812909286440996046838170372310657442173851215723574753731922832154793221423104

    describe "comparison" $ do
      let int LT = -1 :: Int
          int EQ = 0
          int GT = 1
      it "should compare integers" $ properly $ \(NonNegative a, NonNegative b) ->
        withBigNats [ ("a", N a), ("b", N b) ] [ "return M.compare(a, b)" ] >>= flip shouldBe (int $ a `compare` b)
      it "should compare huge integers" $ properly $ \(a, b) ->
        withBigNats [ ("a", a), ("b", b) ] [ "return M.compare(a, b)" ] >>= flip shouldBe (int $ a `compare` b)

      base <- runIO (evalAndPeek @Integer "M.default_base")

      xit "should compare numbers with differing amount of trailing zeroes (example)" $ do
        let a = N $ evalInBase base [0, 1]
            b = N $ evalInBase base [1, 1]
        withBigNats [ ("a", a), ("b", b) ] [ "return M.compare(a, b)" ] >>= flip shouldBe (int $ a `compare` b)

      xit "should compare numbers with differing amount of trailing zeroes (arbitrary)" $
        let g = do
              n <- getNonNegative <$> arbitrary
              m <- chooseInt (0, n)
              suffix <- genDigits base m
              a <- genDigitsWithLeadingZeroes base (n-m)
              b <- genDigitsWithLeadingZeroes base (n-m)
              return (a ++ suffix, b ++ suffix) in
        properly $ forAll g $ \(a, b) ->
          let a' = evalInBase base a in
          let b' = evalInBase base b in
          withBigNats [ ("a", N a'), ("b", N b') ] [ "return M.compare(a, b)" ] >>= flip shouldBe (int $ a' `compare` b')
