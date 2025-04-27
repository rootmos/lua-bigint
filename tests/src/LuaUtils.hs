module LuaUtils where

import Control.Monad ( unless )
import Control.Monad.IO.Class ( MonadIO )
import Data.List ( intercalate )
import System.FilePath ( (</>) )
import System.IO.Unsafe ( unsafePerformIO )
import Text.Printf ( printf )

import Test.QuickCheck

import HsLua hiding ( Integer, error )
import qualified HsLua

--import qualified Data.ByteString as BS
import Data.ByteString.UTF8 as BSUTF8

prependToLuaPath :: FilePath -> String -> String
prependToLuaPath dir path =
  let d0 = dir </> "?.lua" in
  let d1 = dir </> "?" </> "init.lua" in
  intercalate ";" [d0, d1, path]

ensureStackDiff :: LuaError e => Int -> LuaE e a -> LuaE e a
ensureStackDiff d f = do
  before <- fromStackIndex <$> gettop
  a <- f
  after <- fromStackIndex <$> gettop
  let expected = fromIntegral d + before
  if after == expected then return a
  else failLua $ printf "unexpected stack differential: %d != %d" (fromIntegral @_ @Int after) (fromIntegral @_ @Int expected)

stackNeutral :: LuaError e => LuaE e a -> LuaE e a
stackNeutral f = do
  before <- fromStackIndex <$> gettop
  a <- f
  after <- fromStackIndex <$> gettop
  if after == before then return a
  else failLua $ printf "not stack-netural! %d" (fromIntegral @_ @Int $ after - before)

require :: LuaError e => String -> LuaE e StackIndex
require modname = ensureStackDiff 1 $ do
  t <- getglobal "require"
  unless (t == TypeFunction) $ throwTypeMismatchError "function" top
  pushstring $ BSUTF8.fromString modname
  call 1 1
  gettop

requireG :: LuaError e => Name -> String -> LuaE e ()
requireG g modname = stackNeutral $ do
  _ <- require modname
  setglobal g

extendLuaPath :: LuaError e => FilePath -> LuaE e ()
extendLuaPath dir = stackNeutral $ do
  openpackage

  t <- getfield top "path"
  unless (t == TypeString) $ throwTypeMismatchError "string" top
  path <- BSUTF8.toString <$> tostring' top
  pop 2

  let path' = prependToLuaPath dir path

  pushstring $ BSUTF8.fromString path'
  setfield (nth 2) "path"
  pop 1


type Prepare = LuaE HsLua.Exception ()
type RunLuaRun = forall a m. MonadIO m => LuaE HsLua.Exception a -> m a
type RunLuaAndPeek = forall a m. (Peekable a, MonadIO m) => [ String ] -> m a
type EvalLuaAndPeek = forall a m. (Peekable a, MonadIO m) => String -> m a

mkRun :: Prepare -> RunLuaRun
mkRun prepare m = liftIO . HsLua.run $ stackNeutral prepare >> m

mkRunAndPeek :: RunLuaRun -> RunLuaAndPeek
mkRunAndPeek runner ls = runner $ stackNeutral $ do
  mapM_ dostring' ls
  peek top <* pop 1

mkEvalAndPeek :: RunLuaAndPeek -> EvalLuaAndPeek
mkEvalAndPeek runner expr = runner [ "return " ++ expr ]

data LuaBits = Lua32 | Lua64 deriving ( Show, Eq )

luaBits :: LuaBits
luaBits = unsafePerformIO $ HsLua.run @HsLua.Exception $ do
  stackNeutral $ openmath >> setglobal "math"
  dostring' finderOuter
  b <$> peek top
  where finderOuter = "local intmax = 0x7fffffffffffffff\n\
                      \if math.type(intmax) == 'integer' and intmax + 1 < 0 then return 64 end\n\
                      \intmax = 0x7fffffff\n\
                      \if math.type(intmax) == 'integer' and intmax + 1 < 0 then return 32 end\n\
                      \error('unknown integer width')"
        b (32 :: Int) = Lua32
        b 64 = Lua64
        b _ = undefined

dostring' :: LuaError e => String -> LuaE e ()
dostring' s = dostring (BSUTF8.fromString s) >>= \case
  OK -> return ()
  ErrRun -> throwErrorAsException
  _ -> undefined

newtype LuaInt = LuaInt HsLua.Integer deriving ( Show, Num, Eq, Ord )

instance Arbitrary LuaInt where
  arbitrary = do
    let e :: Int = case luaBits of Lua32 -> 31; Lua64 -> 63
    let m :: Int = 2^e
    i <- chooseInt (-m + 1, m - 1)
    return (LuaInt . HsLua.Integer . fromIntegral $ i)

luaIntToInteger :: LuaInt -> Integer
luaIntToInteger (LuaInt (HsLua.Integer i)) = fromIntegral i

expectError :: (LuaError e) => LuaE e Status -> LuaE e (Maybe e)
expectError expr = try expr >>= \case
  Right ErrRun -> Just <$> popException
  Right _ -> return Nothing
  Left _ -> return Nothing
