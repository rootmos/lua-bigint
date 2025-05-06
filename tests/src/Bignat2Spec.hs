module Bignat2Spec where

import Control.Monad ( when )
import Data.Maybe ( fromMaybe )
import System.IO.Unsafe ( unsafePerformIO )
import System.Random ( randomIO )
import Text.Printf
import Data.Ratio ( (%) )

import Test.Hspec
import Test.QuickCheck

import HsLua hiding ( Integer, compare )
import HsLua.Marshalling.Peekers

import Huge
import IntegerLike
import LuaBigInt
import LuaUtils
import Utils

runLua :: RunLuaRun
runLua = mkRun $ do
  prepare
  "N" `requireG` "bignat"

type Base = Integer

data Operand = OpI (Maybe Base) Integer
             | OpH (Maybe Base) Huge
             | OpL LuaInt
             | OpO Base Int Integer

             deriving ( Show )
--instance Show Operand where
  --show = show . operandToInteger

operandToInteger :: Operand -> Integer
operandToInteger (OpI _ i) = i
operandToInteger (OpH _ h) = getHuge h
operandToInteger (OpL li) = luaIntToInteger li
operandToInteger (OpO b o i) = (b^o) * i

instance Eq Operand where
  a == b = operandToInteger a == operandToInteger b

instance Ord Operand where
  compare a b = compare (operandToInteger a) (operandToInteger b)

instance Num Operand where
  a + b = OpI Nothing $ operandToInteger a + operandToInteger b
  a * b = OpI Nothing $ operandToInteger a * operandToInteger b
  abs a = OpI Nothing $ abs $ operandToInteger a
  signum a = OpI Nothing $ signum $ operandToInteger a
  negate o = OpI Nothing (negate $ operandToInteger o)
  fromInteger i = OpI Nothing i

instance Real Operand where
  toRational op = operandToInteger op % 1

instance Enum Operand where
  toEnum = fromInteger . fromIntegral
  fromEnum = undefined

instance Integral Operand where
  toInteger = operandToInteger
  quotRem a b =
    let (q, r) = operandToInteger a `quotRem` operandToInteger b in
    (fromInteger q, fromInteger r)

instance Peekable Operand where
  safepeek idx = retrieving "operand" $ do
    n <- peekFieldRaw peekIntegral "n" idx
    as <- flip mapM [1..n] $ \i -> do
      peekIndexRaw i (\idx -> fromMaybe 0 <$> peekNilOr peekIntegral idx) idx
    o :: Integer <- peekFieldRaw peekIntegral "o" idx
    b <- peekFieldRaw peekIntegral "base" idx
    return $ OpI (Just b) $ (* b^o) $ evalInBase b as

instance Pushable Operand where
  push (OpI Nothing i) = dostring' (printf "return N.fromstring('%s')" (show i))
  push (OpI (Just b) i) = dostring' (printf "return N.fromstring('%s', 10, %d)" (show i) b)
  push (OpH Nothing h) = dostring' (printf "return N.fromstring('%s')" (show $ getHuge h))
  push (OpH (Just b) h) = dostring' (printf "return N.fromstring('%s', 10, %d)" (show $ getHuge h) b)
  push (OpL (LuaInt li)) = pushinteger li
  push (OpO b o i) = ensureStackDiff 1 $ do
    dostring' "return N.make"

    let ds = digitsInBase b i
        n = length ds

    createtable n 3
    t <- gettop

    pushinteger $ fromIntegral b
    setfield t "base"

    flip mapM_ (zip ds [1..]) $ \(a, k) -> stackNeutral $ do
      pushinteger (fromIntegral a)
      rawseti t k

    when (o /= 0 || unsafePerformIO randomIO) $ stackNeutral $ do
      pushinteger $ fromIntegral o
      setfield t "o"

    when (unsafePerformIO randomIO) $ stackNeutral $ do
      pushinteger $ fromIntegral n
      setfield t "n"

    call 1 1

instance Arbitrary Operand where
  arbitrary = frequency [ (50, genI)
                        , (20, genH)
                        , (10, genL)
                        , (20, genO)
                        ]
    where genBase = chooseInteger (2, maxBase)
          genBaseM = oneof [ return Nothing, Just <$> genBase ]
          genI = OpI <$> genBaseM <*> (getNonNegative <$> arbitrary)
          genH = OpH <$> genBaseM <*> arbitrary
          genL = OpL <$> getNonNegative <$> arbitrary
          genO = do
            b <- genBase
            o <- getNonNegative <$> arbitrary
            i <- operandToInteger <$> oneof [ genI, genH ]
            return $ OpO b o i

  shrink (OpI mb i) = OpI mb <$> shrink i
  shrink (OpH mb h) = OpH mb <$> shrink h
  shrink (OpL li) = OpL <$> shrink li
  shrink (OpO b o i) = do
    o' <- shrink o
    i' <- shrink i
    b' <- filter (>= 2) $ shrink b
    return $ OpO b' o' i'

instance IsLuaNative Operand where
  isLuaNative (OpL _) = True
  isLuaNative _ = False

spec :: Spec
spec = do
  it "should have the expected max_base" $ do
    b <- runLua $ return' "N.max_base"
    b `shouldBe` maxBase

  it "should survive a push and peek roundtrip" $ properly $ unary @Operand $ \a -> do
      b <- runLua $ push a >> peek'
      b `shouldBe` a

  integerLike @Operand runLua truncatingSubtraction

  describe "integer conversion" $ do
    it "should convert from non-negative integers" $ properly $ \(NonNegative a) -> do
      a' <- runLua $ do
        "a" `bind` a
        return' "N.frominteger(a)"
      a' `shouldBe` OpL a

    it "should refuse to convert negative integers" $ properly $ \(Negative (a :: LuaInt)) -> do
      Just (Exception msg) <- runLua $ do
        "a" `bind` a
        expectError (dostring "N.frominteger(a)")
      msg `shouldEndWith` "unexpected negative integer"

    it "should safely try converting to native integers" $ properly $ unary @Operand $ \a -> do
      a' <- runLua $ do
        "a" `bind` a
        dostring' "return a:tointeger()"
        isnil top >>= \case
          True -> return Nothing
          False -> Just <$> peek' @Integer
      a' `shouldBe` (if a <= maxint then Just (toInteger a) else Nothing)

  describe "representations" $ do
    describe "decimal" $ do
      it "should render decimal strings" $ properly $ unary @(NonNegative Operand) $ \(NonNegative a) -> do
        s <- runLua $ do
          "a" `bind` a
          return' "a:tostring()"
        s `shouldBe` (show $ toInteger a)

      it "should parse decimal strings" $ properly $ \(NonNegative (a :: Operand)) -> do
        a' <- runLua $ return' $ printf "N.fromstring('%s')" (show $ toInteger a)
        a' `shouldBe` a

    --describe "hexadecimal" $ do
      --it "should reproduce hexadecimal strings" $ properly $ \(NonNegative (n :: Integer)) ->
        --evalAndPeek (printf "M.fromhex('%s'):tohex()" (toHex n)) >>= flip shouldBe (toHex n)
      --it "should reproduce hexadecimal strings of huge integers" $ properly $ \(Huge { getHuge = n }) ->
        --evalAndPeek (printf "M.fromhex('%s'):tohex()" (toHex n)) >>= flip shouldBe (toHex n)
      --it "should produce hexadecimal strings of integers pushed from Haskell" $ properly $ \(a@(N n)) ->
        --withBigNats [ ("a", a) ] [ "return a:tohex()" ] >>= flip shouldBe (toHex n)

    --describe "big-endian" $ do
      --it "should parse big-endian bytestrings" $ properly $ \(Huge { getHuge = n }) -> do
        --(N m) <- runLua $ do
          --stackNeutral $ pushstring (toBeBytes n) >> setglobal "a"
          --dostring' "return M.frombigendian(a)"
          --peek top
        --m `shouldBe` n
      --it "should produce big-endian bytestrings" $ properly $ \(Huge { getHuge = n}) -> do
        --withBigNats [ ("a", N n) ] [ "return a:tobigendian()" ] >>= flip shouldBe (toBeBytes n)

    --describe "little-endian" $ do
      --it "should parse little-endian bytestrings" $ properly $ \(Huge { getHuge = n }) -> do
        --(N m) <- runLua $ do
          --stackNeutral $ pushstring (toLeBytes n) >> setglobal "a"
          --dostring' "return M.fromlittleendian(a)"
          --peek top
        --m `shouldBe` n
      --it "should produce little-endian bytestrings" $ properly $ \(Huge { getHuge = n}) -> do
        --withBigNats [ ("a", N n) ] [ "return a:tolittleendian()" ] >>= flip shouldBe (toLeBytes n)
