module PolynomialSpec where

import Control.Monad ( unless )
import Control.Monad.IO.Class ( MonadIO )
import System.Environment ( lookupEnv )

import qualified Data.ByteString as BS

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

makePolynomial :: (LuaError e, Integral i) => [i] -> LuaE e ()
makePolynomial coeff = ensureStackDiff 1 $ do
  polynomial <- require "polynomial"
  t <- getfield polynomial "make"
  unless (t == TypeFunction) $ throwTypeMismatchError "function" top
  remove 1

  newtable
  p <- gettop

  flip mapM_ (zip coeff [1..]) $ \(a, k) -> do
    pushinteger (fromIntegral a)
    rawseti p k

  call 1 1

withPolynomial :: (Peekable a, MonadIO m, Integral i)
               => [ (Name, [i]) ] -> [ BS.ByteString ] -> m a
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
        let shouldEvaluateTo expr res =
              doRun [ "p = P.make{7,0,9}", "return " <> expr ] >>= flip shouldBe res
        it "should have the correct type" $ do
          "P.is_polynomial(p)" `shouldEvaluateTo` True
        it "should have the correct coefficients" $ do
          "p" `shouldEvaluateTo` ([7, 0, 9] :: [Int])
        it "should have the correct order" $
          "p.n" `shouldEvaluateTo` (3 :: Int)
        it "should have the correct offset" $
          "p.o" `shouldEvaluateTo` (0 :: Int)

      describe "polynomials from Haskell" $ do
        let shouldEvaluateTo expr res =
              withPolynomial [ ("p", [ 7, 0, 9 ] :: [Int]) ] [ "return " <> expr ] >>= flip shouldBe res
        it "should have the correct type" $
          "P.is_polynomial(p)" `shouldEvaluateTo` True
        it "should have the correct coefficients" $ do
          "p" `shouldEvaluateTo` ([7, 0, 9] :: [Int])
        it "should have the correct order" $
          "p.n" `shouldEvaluateTo` (3 :: Int)
        it "should have the correct offset" $
          "p.o" `shouldEvaluateTo` (0 :: Int)

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
