module LuaUtils where

import Control.Monad ( unless )
import Control.Monad.IO.Class ( MonadIO )
import Data.Function ( (&) )
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

bind :: (LuaError e, Pushable a) => Name -> a -> LuaE e ()
bind n a = stackNeutral $ push a >> setglobal n

require :: LuaError e => String -> LuaE e ()
require modname = ensureStackDiff 1 $ do
  t <- getglobal "require"
  unless (t == TypeFunction) $ throwTypeMismatchError "function" top
  pushstring $ BSUTF8.fromString modname
  call 1 1

requireG :: LuaError e => Name -> String -> LuaE e ()
requireG g modname = stackNeutral $ require modname >> setglobal g

extendLuaPath :: LuaError e => FilePath -> LuaE e ()
extendLuaPath dir = stackNeutral $ do
  t <- getglobal "package"
  unless (t == TypeTable) $ throwTypeMismatchError "table" top

  t <- getfield top "path"
  unless (t == TypeString) $ throwTypeMismatchError "string" top
  path <- BSUTF8.toString <$> tostring' top
  pop 2

  let path' = prependToLuaPath dir path

  pushstring $ BSUTF8.fromString path'
  setfield (nth 2) "path"
  pop 1


type Prepare = Lua ()
type RunLuaRun = forall m a. MonadIO m => Lua a -> m a
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

handleStatus :: LuaError e => Status -> LuaE e ()
handleStatus OK = return ()
handleStatus ErrRun = throwErrorAsException
handleStatus e = error $ show e

dostring' :: LuaError e => String -> LuaE e ()
dostring' s = dostringTrace (BSUTF8.fromString s) >>= handleStatus

peek' :: (Peekable a, LuaError e) => LuaE e a
peek' = ensureStackDiff (-1) $ peek top <* pop 1

return' :: (LuaError e, Peekable a) => String -> LuaE e a
return' expr = stackNeutral $ do
  dostring' $ "return " ++ expr
  peek'

newtype LuaInt = LuaInt HsLua.Integer deriving ( Show, Num, Eq, Ord )

maxint :: Integral a => a
maxint = case luaBits of { Lua32 -> 31 :: Integer ; Lua64 -> 63 } & \b -> 2^b - 1

minint :: Integral a => a
minint = case luaBits of { Lua32 -> 31 :: Integer ; Lua64 -> 63 } & \b -> -2^b

instance Arbitrary LuaInt where
  arbitrary = fmap (LuaInt . HsLua.Integer . fromIntegral) $
    frequency [ (5, elements [ -1, 0, 1, minint, maxint ]) -- the usual suspects
              , (10, chooseInt (-0x10, 0x10))
              , (20, chooseInt (-0x100, 0x100))
              , (20, chooseInt (-0x1000, 0x1000))
              , (45, chooseInt (minint, maxint))
              ]

  shrink li = LuaInt . fromIntegral <$> shrink (luaIntToInteger li)

instance Pushable LuaInt where
  push (LuaInt i) = pushinteger i

luaIntToInteger :: LuaInt -> Integer
luaIntToInteger (LuaInt (HsLua.Integer i)) = fromIntegral i

expectError :: (LuaError e) => LuaE e Status -> LuaE e (Maybe e)
expectError expr = try expr >>= \case
  Right ErrRun -> Just <$> popException
  Right _ -> return Nothing
  Left _ -> return Nothing

expectError' :: (LuaError e) => String -> LuaE e (Maybe e)
expectError' s = expectError $ dostring . BSUTF8.fromString $ "return " ++ s
