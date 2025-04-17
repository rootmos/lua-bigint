module BignatSpec where

import Control.Monad ( forM_ )
import Data.Maybe ( isJust, fromJust )
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

newtype BigNat = N Integer deriving ( Show, Eq, Ord, Num, Real, Enum, Integral )

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
      it (printf "should set the expected max and default bases (2^e = 2^%d = 0x%x)" e ((2 :: Int)^e)) $ do
        evalAndPeek @Integer "M.max_base" >>= flip shouldBe (2^e)
        evalAndPeek @Integer "M.default_base" >>= flip shouldBe (2^e)

    base <- runIO (evalAndPeek @Integer "M.default_base")

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


      it (printf "should compare numbers with differing amount of zeroes in base 0x%x (example)" base) $ do
        let a = N $ evalInBase base [0, 1]
            b = N $ evalInBase base [1, 1]
        withBigNats [ ("a", a), ("b", b) ] [ "return M.compare(a, b)" ] >>= flip shouldBe (int $ a `compare` b)

      it (printf "should compare numbers with differing amount of zeroes in base 0x%x (arbitrary)" base) $
        let g = do
              n <- getNonNegative <$> arbitrary
              m <- chooseInt (0, n)
              suffix <- genDigits base m
              a <- genDigitsWithLeadingZeroes base (n-m)
              b <- genDigitsWithLeadingZeroes base (n-m)
              return (a ++ suffix, b ++ suffix)
        in properly $ forAll g $ \(a, b) ->
          let a' = evalInBase base a in
          let b' = evalInBase base b in
          withBigNats [ ("a", N a'), ("b", N b') ] [ "return M.compare(a, b)" ] >>= flip shouldBe (int $ a' `compare` b')

    describe "operators" $ do
      let operators :: [ (String, Bool, (forall a. (Eq a, Ord a) => a -> a -> Bool)) ]
          operators = [ ("==", True, (==))
                      , ("~=", False, (/=))
                      , ("<", False, (<))
                      , ("<=", True, (<=))
                      , (">", False, (>))
                      , (">=", True, (>=))
                      ]
      forM_ operators $ \(oplua, refl, op) -> do
        describe oplua $ do
          it (printf "should %s reflexive for integers (by reference)" (be refl)) $ properly $ \(NonNegative a) ->
            withBigNats [ ("a", N a) ] [ printf "return a %s a" oplua ] >>= flip shouldBe refl
          it (printf "should %s reflexive for huge integers (by reference)" (be refl)) $ properly $ \a ->
            withBigNats [ ("a", a) ] [ printf "return a %s a" oplua ] >>= flip shouldBe refl

          it (printf "should %s reflexive for integers (by value)" (be refl)) $ properly $ \(NonNegative a) ->
            withBigNats [ ("a", N a), ("b", N a) ] [ printf "return a %s b" oplua ] >>= flip shouldBe refl
          it (printf "should %s reflexive for huge integers (by value)" (be refl)) $ properly $ \a ->
            withBigNats [ ("a", a), ("b", a) ] [ printf "return a %s b" oplua ] >>= flip shouldBe refl

          it "should work for integers" $ properly $ \(NonNegative a, NonNegative b) ->
            withBigNats [ ("a", N a), ("b", N b) ] [ printf "return a %s b" oplua ] >>= flip shouldBe (op a b)
          it "should work for huge integers" $ properly $ \(a, b) ->
            withBigNats [ ("a", a), ("b", b) ] [ printf "return a %s b" oplua ] >>= flip shouldBe (op a b)

    describe "truncating subtraction" $ do
      let sub a b | a <= b = 0
                  | otherwise = a - b
      it "should subtract integers" $ properly $ \(NonNegative a, NonNegative b) -> do
        withBigNats [ ("a", N a), ("b", N b) ] [ "d, _ = M.sub(a, b)", "return d" ] >>= flip shouldBe (N $ sub a b)
        withBigNats [ ("a", N a), ("b", N b) ] [ "_, t = M.sub(a, b)", "return t" ] >>= flip shouldBe (a < b)
      it "should subtract huge integers" $ properly $ \(a, b) -> do
        withBigNats [ ("a", a), ("b", b) ] [ "d, _ = M.sub(a, b)", "return d" ] >>= flip shouldBe (sub a b)
        withBigNats [ ("a", a), ("b", b) ] [ "_, t = M.sub(a, b)", "return t" ] >>= flip shouldBe (a < b)

      it "should __sub integers" $ properly $ \(a, b) ->
        withBigNats [ ("a", a), ("b", b) ] [ "return a - b" ] >>= flip shouldBe (sub a b)

      it "should subtract same integer (by value)" $ properly $ \(NonNegative a) -> do
        withBigNats [ ("a", N a), ("b", N a) ] [ "d, _ = M.sub(a, b)", "return d" ] >>= flip shouldBe (N 0)
        withBigNats [ ("a", N a), ("b", N a) ] [ "_, t = M.sub(a, b)", "return t" ] >>= flip shouldBe False
      it "should subtract same huge integer (by value)" $ properly $ \a -> do
        withBigNats [ ("a", a), ("b", a) ] [ "d, _ = M.sub(a, b)", "return d" ] >>= flip shouldBe (N 0)
        withBigNats [ ("a", a), ("b", a) ] [ "_, t = M.sub(a, b)", "return t" ] >>= flip shouldBe False
      it "should subtract same integer (by reference)" $ properly $ \(NonNegative a) -> do
        withBigNats [ ("a", N a) ] [ "d, _ = M.sub(a, a)", "return d" ] >>= flip shouldBe (N 0)
        withBigNats [ ("a", N a) ] [ "_, t = M.sub(a, a)", "return t" ] >>= flip shouldBe False
      it "should subtract same huge integer (by reference)" $ properly $ \a -> do
        withBigNats [ ("a", a) ] [ "d, _ = M.sub(a, a)", "return d" ] >>= flip shouldBe (N 0)
        withBigNats [ ("a", a) ] [ "_, t = M.sub(a, a)", "return t" ] >>= flip shouldBe False

    describe "division" $ do
      describe "examples" $ do
        let fromstring base ds = printf "M.fromstring(\"%s\",%d,%d)" ds base base
        let example (base :: Int) (a :: String) (b :: String) (q :: String) (r :: String) =
             let a' :: String = fromstring base a in let b' :: String = fromstring base b in
             let expr = printf "M.divrem(%s, %s)" a' b' in
             it (printf "%s should be %s, %s" expr q r) $ do
               runAndPeek [ "q, _ = " ++ expr, printf "return q:tostring(%d)" base ] >>= flip shouldBe q
               runAndPeek [ "_, r = " ++ expr, printf "return r:tostring(%d)" base ] >>= flip shouldBe r

        -- https://en.wikipedia.org/wiki/Long_division#Examples
        example 10 "1260257" "37" "34061" "0"
        example 16 "f412df" "12" "d8f45" "5"

        example 10 "12" "5" "2" "2"
        example 10 "1200" "50" "24" "0"

      it "should divide integers" $ properly $ \(NonNegative a, Positive b) -> do
        let (q, r) = a `divMod` b
        withBigNats [ ("a", N a), ("b", N b) ] [ "q, _ = M.divrem(a, b)", "return q" ] >>= flip shouldBe (N q)
        withBigNats [ ("a", N a), ("b", N b) ] [ "_, r = M.divrem(a, b)", "return r" ] >>= flip shouldBe (N r)
      it "should divide huge integers" $ properly $ \(a, b) -> do
        let (q, r) = a `divMod` b
        withBigNats [ ("a", a), ("b", b) ] [ "q, _ = M.divrem(a, b)", "return q" ] >>= flip shouldBe q
        withBigNats [ ("a", a), ("b", b) ] [ "_, r = M.divrem(a, b)", "return r" ] >>= flip shouldBe r

      it "should __idiv integers" $ properly $ \(a, b) ->
        withBigNats [ ("a", a), ("b", b) ] [ "return a // b" ] >>= flip shouldBe (a `div` b)

      it "should __mod integers" $ properly $ \(a, b) ->
        withBigNats [ ("a", a), ("b", b) ] [ "return a % b" ] >>= flip shouldBe (a `mod` b)

      it (printf "should divide numbers with lots zeroes in base 0x%x" base) $
        let g = do
              a <- getNonNegative <$> arbitrary >>= genDigitsWithLeadingZeroes base
              b <- getNonNegative <$> arbitrary >>= genDigitsWithLeadingZeroes base
              if sum b == 0 then discard else return (a, b)
        in properly $ forAll g $ \(a, b) ->
          let a' = evalInBase base a in
          let b' = evalInBase base b in
          let (q, r) = a' `divMod` b' in do
          withBigNats [ ("a", N a'), ("b", N b') ] [ "q, _ = M.divrem(a, b)", "return q" ] >>= flip shouldBe (N q)
          withBigNats [ ("a", N a'), ("b", N b') ] [ "_, r = M.divrem(a, b)", "return r" ] >>= flip shouldBe (N r)

      it "should dislike dividing by zero" $ properly $ \a -> do
        e <- runLua $ do
          pushBigNat a
          setglobal "a"
          try (dostring "M.divrem(a, M{0})") >>= \case
            Right ErrRun -> Just <$> popException
            Right _ -> return Nothing
            Left _ -> return Nothing
        e `shouldSatisfy` isJust
        let Exception msg = fromJust e
        msg `shouldEndWith` "attempt to divide by zero"

      -- TODO same integer
