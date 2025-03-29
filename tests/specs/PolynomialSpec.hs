module PolynomialSpec where

import Control.Monad ( unless, when )
import Data.List ( dropWhileEnd )
import Data.Maybe ( fromMaybe )
import Text.Printf

--import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BSUTF8

import Test.Hspec
import Test.QuickCheck

import HsLua hiding ( property )
import HsLua.Marshalling.Peekers

import LuaBigInt
import LuaUtils

runLua :: RunLuaRun
runLua = mkRun $ do
  prepare
  "P" `requireG` "polynomial"

runAndPeek :: RunLuaAndPeek
runAndPeek = mkRunAndPeek runLua

data Polynomial = P { coefficients :: [Int]
                    , offset :: Int
                    }
                  deriving ( Show, Eq )

instance Arbitrary Polynomial where
  arbitrary = P <$> arbitrary <*> (getNonNegative <$> arbitrary)

instance Peekable Polynomial where
  safepeek idx = retrieving "Polynomial" $ do
    n <- peekFieldRaw peekIntegral "n" idx
    as <- flip mapM [1..n] $ \i -> do
      peekIndexRaw i (\idx -> fromMaybe 0 <$> peekNilOr peekIntegral idx) idx
    o <- peekFieldRaw peekIntegral "o" idx
    return $ P as o

pushPolynomial :: LuaError e => Polynomial -> LuaE e ()
pushPolynomial p = ensureStackDiff 1 $ do
  polynomial <- require "polynomial"
  t <- getfield polynomial "make"
  unless (t == TypeFunction) $ throwTypeMismatchError "function" top
  remove 1

  newtable
  t <- gettop

  flip mapM_ (zip (coefficients p) [1..]) $ \(a, k) -> stackNeutral $ do
    pushinteger (fromIntegral a)
    rawseti t k

  when (offset p /= 0) $ stackNeutral $ do
    pushinteger (fromIntegral $ offset p)
    setfield t "o"

  call 1 1

withPolynomial :: [ (Name, Polynomial) ] -> RunLuaAndPeek
withPolynomial ps ls = runLua $ stackNeutral $ do
  flip mapM_ ps $ \(n, p) -> stackNeutral $ do
    pushPolynomial p
    setglobal n

  flip mapM_ ls $ \l -> dostring (BSUTF8.fromString l) >>= \case
    OK -> return ()
    ErrRun -> throwErrorAsException
    _ -> undefined

  a <- peek top
  pop 1
  return a

inspectPolynomial :: (forall a. (Eq a, Show a, Peekable a) => String -> a -> IO ()) -> String -> Polynomial -> SpecWith ()
inspectPolynomial shouldEvaluateTo name p = context (printf "%s == %s" name (show p)) $ do
  it "should have the correct type" $ do
    (printf "P.is_polynomial(%s)" name) `shouldEvaluateTo` True
  it "should have the correct coefficients" $ do
    (printf "%s" name) `shouldEvaluateTo` (coefficients p)
  it "should have the correct order" $
    (printf "%s.n" name) `shouldEvaluateTo` (length $ coefficients p)
  it "should have the correct offset" $
    (printf "%s.o" name) `shouldEvaluateTo` (offset p)

clean :: Polynomial -> Polynomial
clean = zo . stripLeadingZeroes . stripTrailingZeroes
  where stripTrailingZeroes (P as o) = P (dropWhileEnd (== 0) as) o
        stripLeadingZeroes (P [] o) = P [] o
        stripLeadingZeroes (P (0:as) o) = stripLeadingZeroes $ P as (succ o)
        stripLeadingZeroes (P as@(_:_) o) = P as o
        zo (P [] _) = P [] 0
        zo p@(P _ _) = p

spec :: Spec
spec = do
  describe "polynomial.lua" $ do
    it "should prepare properly" $ do
      t <- runLua $ do
        OK <- dostring "return type(P)"
        peek @String top
      t `shouldBe` "table"

    describe "make" $ do
      describe "polynomials from inside Lua" $ do
        let shouldEvaluateTo make expr res =
              runAndPeek [ "p = " <> make, "return " <> expr ] >>= flip shouldBe res
            testCase make expect = context ("p := " ++ make) $ do
              inspectPolynomial (shouldEvaluateTo make) "p" expect

        testCase "P.make{}" (P [] 0)
        testCase "P.make{1}" (P [1] 0)
        testCase "P.make{7,0,9}" (P [7,0,9] 0)
        testCase "P.make{10,11,o=1}" (P [10,11] 1)
        testCase "P.make{12,nil,13,n=3}" (P [12,0,13] 0)

        testCase "P.make{0,1}" (P [1] 1)
        testCase "P.make{0,0,1}" (P [1] 2)
        testCase "P.make{nil,1,n=2}" (P [1] 1)
        testCase "P.make{nil,nil,1,n=3}" (P [1] 2)
        testCase "P.make{0,0,1,0,2}" (P [1,0,2] 2)
        testCase "P.make{nil,nil,1,0,2,n=5}" (P [1,0,2] 2)

        testCase "P.make{0}" (P [] 0)
        testCase "P.make{0,0,0}" (P [] 0)
        testCase "P.make{1,0}" (P [1] 0)
        testCase "P.make{1,0,0}" (P [1] 0)

        testCase "P.make{0,1,0}" (P [1] 1)
        testCase "P.make{0,0,1,0,0}" (P [1] 2)
        testCase "P.make{0,0,1,0,2,0,0,0}" (P [1,0,2] 2)

      describe "polynomials from Haskell" $ do
        let shouldEvaluateTo name p expr res =
              withPolynomial [ (name, p) ] [ "return " <> expr ] >>= flip shouldBe res
            testCase2 p expect = context ("p := " ++ show p) $ do
              inspectPolynomial (shouldEvaluateTo "p" p) "p" expect
            testCase p = testCase2 p p

        testCase (P [] 0)
        testCase (P [1] 0)
        testCase (P [7,0,9] 0)
        testCase (P [10,11] 1)

        testCase2 (P [0,1] 0) (P [1] 1)
        testCase2 (P [0,0,1] 0) (P [1] 2)
        testCase2 (P [0,0,1,0,2] 0) (P [1,0,2] 2)

        testCase2 (P [0] 0) (P [] 0)
        testCase2 (P [0,0,0] 0) (P [] 0)
        testCase2 (P [1,0] 0) (P [1] 0)
        testCase2 (P [1,0,0] 0) (P [1] 0)

        testCase2 (P [0,1,0] 0) (P [1] 1)
        testCase2 (P [0,0,1,0,0] 0) (P [1] 2)
        testCase2 (P [0,0,1,0,2,0,0,0] 0) (P [1,0,2] 2)

    describe "Peekable Polynomial" $ do
      describe "polynomials from inside Lua" $ do
        let testCase make expect = it ("should peek " ++ make) $ do
              runAndPeek [ "return " <> make ] >>= flip shouldBe expect

        testCase "P.make{}" (P [] 0)
        testCase "P.make{1}" (P [1] 0)
        testCase "P.make{7,0,9}" (P [7,0,9] 0)
        testCase "P.make{10,11,o=1}" (P [10,11] 1)
        testCase "P.make{12,nil,13,n=3}" (P [12,0,13] 0)

        testCase "P.make{0,1}" (P [1] 1)
        testCase "P.make{0,0,1}" (P [1] 2)
        testCase "P.make{nil,1,n=2}" (P [1] 1)
        testCase "P.make{nil,nil,1,n=3}" (P [1] 2)
        testCase "P.make{0,0,1,0,2}" (P [1,0,2] 2)
        testCase "P.make{nil,nil,1,0,2,n=5}" (P [1,0,2] 2)

        testCase "P.make{0}" (P [] 0)
        testCase "P.make{0,0,0}" (P [] 0)
        testCase "P.make{1,0}" (P [1] 0)
        testCase "P.make{1,0,0}" (P [1] 0)

        testCase "P.make{0,1,0}" (P [1] 1)
        testCase "P.make{0,0,1,0,0}" (P [1] 2)
        testCase "P.make{0,0,1,0,2,0,0,0}" (P [1,0,2] 2)

      it "should push and peek roundtrip" $ property $ \p -> do
        q <- runLua $ pushPolynomial p >> peek top
        q `shouldBe` clean p

    describe "clone" $ do
      let shouldEvaluateTo expr res =
            withPolynomial [ ("x", P [7] 0) ] [ "y = x:clone()", "y[1] = 9", "return " <> expr ] >>= flip shouldBe res
      inspectPolynomial (shouldEvaluateTo ) "x" (P [7] 0)
      inspectPolynomial (shouldEvaluateTo ) "y" (P [9] 0)

      let shouldEvaluateTo expr res =
            withPolynomial [ ("x", P [7] 0) ] [ "y = x:clone()", "x[1] = 9", "return " <> expr ] >>= flip shouldBe res
      inspectPolynomial (shouldEvaluateTo ) "x" (P [9] 0)
      inspectPolynomial (shouldEvaluateTo ) "y" (P [7] 0)

    describe "add" $ do
      describe "polynomials from inside Lua" $ do
        let shouldEvaluateTo (op1 :: String) (op2 :: String) expr res =
              runAndPeek [ printf "sum = %s + %s" op1 op2, "return " <> expr ] >>= flip shouldBe res
            testCase op1 op2 expect = context (printf "sum := %s + %s" op1 op2) $ do
              inspectPolynomial (shouldEvaluateTo op1 op2) "sum" expect

        testCase "P.make{}" "P.make{}" (P [] 0)
        testCase "P.make{1}" "P.make{}" (P [1] 0)
        testCase "P.make{}" "P.make{2}" (P [2] 0)
        testCase "P.make{1,2}" "P.make{3,0,4}" (P [4,2,4] 0)
        testCase "P.make{1,2}" "P.make{3, o=2}" (P [1,2,3] 0)
        testCase "P.make{1,2,o=1}" "P.make{3, o=2}" (P [1,5] 1)
        testCase "P.make{0,1,2}" "P.make{0,0,3}" (P [1,5] 1)

      describe "polynomials from Haskell" $ do
        let shouldEvaluateTo (name :: String) op1 op2 expr res =
              withPolynomial [ ("op1", op1), ("op2", op2) ] [ printf "%s = op1 + op2" name, "return " <> expr ] >>= flip shouldBe res
            testCase op1 op2 expected = context (printf "sum := %s + %s" (show op1) (show op2)) $ do
              inspectPolynomial (shouldEvaluateTo "sum" op1 op2) "sum" expected

        testCase (P [] 0) (P [] 0) (P [] 0)
        testCase (P [1] 0) (P [] 0) (P [1] 0)
        testCase (P [] 0) (P [1] 0) (P [1] 0)
        testCase (P [1,2] 0) (P [3,0,4] 0) (P [4,2,4] 0)
        testCase (P [1,2] 0) (P [3] 2) (P [1,2,3] 0)
        testCase (P [1,2] 1) (P [3] 2) (P [1,5] 1)

      describe "zero" $ do
        it "should be a left identity" $ property $ \p -> do
          q <- withPolynomial [ ("a", p) ] [ "return P.make{0} + a" ]
          q `shouldBe` clean p
        it "should be a right identity" $ property $ \p -> do
          q <- withPolynomial [ ("a", p) ] [ "return a + P.make{0}" ]
          q `shouldBe` clean p

    describe "mul" $ do
      describe "polynomials from inside Lua" $ do
        let shouldEvaluateTo (op1 :: String) (op2 :: String) expr res =
              runAndPeek [ printf "prod = %s * %s" op1 op2, "return " <> expr ] >>= flip shouldBe res
            testCase op1 op2 expect = context (printf "prod := %s + %s" op1 op2) $ do
              inspectPolynomial (shouldEvaluateTo op1 op2) "prod" expect

        testCase "P.make{}" "P.make{}" (P [] 0)
        testCase "P.make{0}" "P.make{2,3}" (P [] 0)
        testCase "P.make{2,3}" "P.make{0}" (P [] 0)

        testCase "P.make{1}" "P.make{1}" (P [1] 0)
        testCase "P.make{1}" "P.make{2,3}" (P [2,3] 0)
        testCase "P.make{2,3}" "P.make{1}" (P [2,3] 0)

        testCase "P.make{2}" "P.make{3}" (P [6] 0)
        testCase "P.make{1,2}" "P.make{3,4}" (P [1*3, 1*4+2*3, 2*4] 0)

        -- (x+2x^3)*(3x^2+4x^5) == x^3*(8x^5 + 4x^3 + 6x^2 + 3)
        testCase "P.make{0,1,0,2}" "P.make{0,0,3,0,0,4}" (P [3, 0, 6, 4, 0, 8] 3)
        testCase "P.make{1,0,2,o=1}" "P.make{3,0,0,4,o=2}" (P [3, 0, 6, 4, 0, 8] 3)

      describe "polynomials from Haskell" $ do
        let shouldEvaluateTo (name :: String) op1 op2 expr res =
              withPolynomial [ ("op1", op1), ("op2", op2) ] [ printf "%s = op1 * op2" name, "return " <> expr ] >>= flip shouldBe res
            testCase op1 op2 expected = context (printf "prod := %s * %s" (show op1) (show op2)) $ do
              inspectPolynomial (shouldEvaluateTo "prod" op1 op2) "prod" expected

        testCase (P [] 0) (P [] 0) (P [] 0)
        testCase (P [0] 0) (P [2,3] 0) (P [] 0)
        testCase (P [2,3] 0) (P [0] 0) (P [] 0)

        testCase (P [1] 0) (P [1] 0) (P [1] 0)
        testCase (P [1] 0) (P [2,3] 0) (P [2,3] 0)
        testCase (P [2,3] 0) (P [1] 0) (P [2,3] 0)

        testCase (P [2] 0) (P [3] 0) (P [6] 0)
        testCase (P [1,2] 0) (P [3,4] 0) (P [1*3, 1*4+2*3, 2*4] 0)

        -- (x+2x^3)*(3x^2+4x^5) == x^3*(8x^5 + 4x^3 + 6x^2 + 3)
        testCase (P [0,1,0,2] 0) (P [0,0,3,0,0,4] 0) (P [3, 0, 6, 4, 0, 8] 3)
        testCase (P [1,0,2] 1) (P [3,0,0,4] 2) (P [3, 0, 6, 4, 0, 8] 3)

      describe "one" $ do
        it "should be a left identity" $ property $ \p -> do
          q <- withPolynomial [ ("a", p) ] [ "return P.make{1} * a" ]
          q `shouldBe` clean p
        it "should be a right identity" $ property $ \p -> do
          q <- withPolynomial [ ("a", p) ] [ "return a * P.make{1}" ]
          q `shouldBe` clean p

      describe "zero" $ do
        it "should be a left zero divisor" $ property $ \p -> do
          q <- withPolynomial [ ("a", p) ] [ "return P.make{0} * a" ]
          q `shouldBe` (P [] 0)
        it "should be a right zero divisor" $ property $ \p -> do
          q <- withPolynomial [ ("a", p) ] [ "return a * P.make{0}" ]
          q `shouldBe` (P [] 0)
