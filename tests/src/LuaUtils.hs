module LuaUtils where

import HsLua hiding ( Integer )

import Control.Monad ( unless )
import Control.Monad.IO.Class ( MonadIO )
import Data.List ( intercalate )
import System.FilePath ( (</>) )
import Text.Printf ( printf )

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
type RunLuaRun = forall m a. MonadIO m => LuaE HsLua.Exception a -> m a
type RunLuaAndPeek = forall m a. (Peekable a, MonadIO m) => [ String ] -> m a

mkRun :: Prepare -> RunLuaRun
mkRun prepare m = liftIO . HsLua.run $ stackNeutral prepare >> m

mkRunAndPeek :: RunLuaRun -> RunLuaAndPeek
mkRunAndPeek runner ls = runner $ stackNeutral $ do
  flip mapM_ ls $ \l -> dostring (BSUTF8.fromString l) >>= \case
    OK -> return ()
    ErrRun -> throwErrorAsException
    _ -> undefined
  a <- peek top
  pop 1
  return a
