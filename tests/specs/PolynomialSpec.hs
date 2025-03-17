module PolynomialSpec where

import Control.Monad ( unless, when )
import Control.Monad.IO.Class ( MonadIO )
import System.Environment ( lookupEnv )
import Text.Printf

import qualified Data.ByteString as BS
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

doRun :: (Peekable a, MonadIO m) => [BS.ByteString] -> m a
doRun ls = liftIO $ HsLua.run @HsLua.Exception $ stackNeutral $ do
  prepare
  flip mapM_ ls $ \l -> dostring l >>= \case
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
               => [ (Name, Polynomial) ] -> [ BS.ByteString ] -> m a
withPolynomial ps ls = liftIO $ HsLua.run @HsLua.Exception $ stackNeutral $ do
  prepare

  flip mapM_ ps $ \(n, p) -> stackNeutral $ do
    makePolynomial p
    setglobal n

  flip mapM_ ls $ \l -> dostring l >>= \case
    OK -> return ()
    ErrRun -> throwErrorAsException
    _ -> undefined

  a <- peek top
  pop 1
  return a


inspectPolynomial :: (forall a. (Eq a, Show a, Peekable a) => BS.ByteString -> a -> IO ()) -> String -> Polynomial -> SpecWith ()
inspectPolynomial shouldEvaluateTo name p = context (show p) $ do
  it "should have the correct type" $ do
    (BSUTF8.fromString $ printf "P.is_polynomial(%s)" name) `shouldEvaluateTo` True
  it "should have the correct coefficients" $ do
    (BSUTF8.fromString $ printf "%s" name) `shouldEvaluateTo` (coefficients p)
  it "should have the correct order" $
    (BSUTF8.fromString $ printf "%s.n" name) `shouldEvaluateTo` (length $ coefficients p)
  it "should have the correct offset" $
    (BSUTF8.fromString $ printf "%s.o" name) `shouldEvaluateTo` (offset p)

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
              doRun [ (BSUTF8.fromString $ printf "%s = P.make%s" name make), "return " <> expr ] >>= flip shouldBe res
            testCase make = inspectPolynomial (shouldEvaluateTo "p" make) "p"

        testCase "{}" (P [] 0)
        testCase "{1}" (P [1] 0)
        testCase "{7,0,9}" (P [7,0,9] 0)
        testCase "{10,11,o=1}" (P [10,11] 1)
        testCase "{12,nil,13,n=3}" (P [12,0,13] 0)

      describe "polynomials from Haskell" $ do
        let shouldEvaluateTo name p expr res =
              withPolynomial [ (name, p) ] [ "return " <> expr ] >>= flip shouldBe res
            testCase p = inspectPolynomial (shouldEvaluateTo "p" p) "p" p

        testCase (P [] 0)
        testCase (P [1] 0)
        testCase (P [7,0,9] 0)
        testCase (P [10,11] 1)

    describe "add" $ do
      describe "polynomials from inside Lua" $ do
        let shouldEvaluateTo expr res =
              doRun [ "sum = P.make{1,2} + P.make{3,0,4}", "return " <> expr ] >>= flip shouldBe res
        it "should have the correct type" $ do
          "P.is_polynomial(sum)" `shouldEvaluateTo` True
        it "should have the correct coefficients" $ do
          "sum" `shouldEvaluateTo` ([4,2,4] :: [Int])
        it "should have the correct order" $ do
          "sum.n" `shouldEvaluateTo` (3 :: Int)
        it "should have the correct offset" $ do
          "sum.o" `shouldEvaluateTo` (0 :: Int)

    --it "should have an identity element" $ property $ \(a :: Int) ->
        --a + 0 `shouldBe` a
    --it "should be commutative" $ property $ \(a :: Int) b ->
        --a + b `shouldBe` b + a
