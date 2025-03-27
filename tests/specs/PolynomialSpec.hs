module PolynomialSpec where

import Control.Monad ( unless, when )
import Control.Monad.IO.Class ( MonadIO )
import System.Environment ( lookupEnv )
import Text.Printf

--import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BSUTF8

import Test.Hspec
--import Test.QuickCheck

import HsLua

import LuaUtils
import Paths_lua_bigint

pickLuaSource :: IO FilePath
pickLuaSource = lookupEnv "LUA_BIGINT_SRC" >>= \case
  Just p -> return p
  Nothing -> Paths_lua_bigint.getDataFileName "lua"

prepare :: LuaError e => LuaE e ()
prepare = stackNeutral $ do
    openlibs
    src <- liftIO pickLuaSource
    extendLuaPath src

    _ <- require "polynomial"
    setglobal "P"

doRun :: (Peekable a, MonadIO m) => [ String ] -> m a
doRun ls = liftIO $ HsLua.run @HsLua.Exception $ stackNeutral $ do
  prepare
  flip mapM_ ls $ \l -> dostring (BSUTF8.fromString l) >>= \case
    OK -> return ()
    ErrRun -> throwErrorAsException
    _ -> undefined
  a <- peek top
  pop 1
  return a

data Polynomial = P { coefficients :: [Int]
                    , offset :: Int
                    }
                  deriving ( Show, Eq )

makePolynomial :: LuaError e => Polynomial -> LuaE e ()
makePolynomial p = ensureStackDiff 1 $ do
  polynomial <- require "polynomial"
  t <- getfield polynomial "make"
  unless (t == TypeFunction) $ throwTypeMismatchError "function" top
  remove 1

  newtable
  t <- gettop

  flip mapM_ (zip (coefficients p) [1..]) $ \(a, k) -> do
    pushinteger (fromIntegral a)
    rawseti t k

  when (offset p /= 0) $ stackNeutral $ do
    pushinteger (fromIntegral $ offset p)
    setfield t "o"

  call 1 1

withPolynomial :: (Peekable a, MonadIO m)
               => [ (Name, Polynomial) ] -> [ String ] -> m a
withPolynomial ps ls = liftIO $ HsLua.run @HsLua.Exception $ stackNeutral $ do
  prepare

  flip mapM_ ps $ \(n, p) -> stackNeutral $ do
    makePolynomial p
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

spec :: Spec
spec = do
  describe "polynomial.lua" $ do
    it "should prepare properly" $ do
      t <- liftIO $ HsLua.run @HsLua.Exception $ do
        prepare
        OK <- dostring "return type(P)"
        peek @String top
      t `shouldBe` "table"

    describe "make" $ do
      describe "polynomials from inside Lua" $ do
        let shouldEvaluateTo (name :: String) (make :: String) expr res =
              doRun [ (printf "%s = %s" name make), "return " <> expr ] >>= flip shouldBe res
            testCase make expect = context ("p := " ++ make) $ do
              inspectPolynomial (shouldEvaluateTo "p" make) "p" expect

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

    describe "add" $ do
      describe "polynomials from inside Lua" $ do
        let shouldEvaluateTo (op1 :: String) (op2 :: String) expr res =
              doRun [ printf "sum = %s + %s" op1 op2, "return " <> expr ] >>= flip shouldBe res
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

    describe "mul" $ do
      describe "polynomials from inside Lua" $ do
        let shouldEvaluateTo (op1 :: String) (op2 :: String) expr res =
              doRun [ printf "prod = %s * %s" op1 op2, "return " <> expr ] >>= flip shouldBe res
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

    --it "should have an identity element" $ property $ \(a :: Int) ->
        --a + 0 `shouldBe` a
    --it "should be commutative" $ property $ \(a :: Int) b ->
        --a + b `shouldBe` b + a
