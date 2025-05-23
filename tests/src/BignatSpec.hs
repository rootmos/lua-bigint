module BignatSpec where

import Control.Monad ( when )
import Data.Maybe ( fromMaybe )
import Data.Ratio ( (%) )
import GHC.Generics ( Generic )
import Generic.Data ( gshowsPrec )
import System.IO.Unsafe ( unsafePerformIO )
import System.Random ( randomIO )
import Text.Printf

import Test.Hspec
import Test.QuickCheck

import HsLua hiding ( Integer, compare )
import HsLua.Marshalling.Peekers

import Huge
import qualified IntegerLike as I
import Lib
import LuaUtils
import Utils

runLua :: RunLuaRun
runLua = mkRun $ prepare' "N" "bignat"

data Operand = OpI (Maybe Base) Integer
             | OpH (Maybe Base) Huge
             | OpL LuaInt
             | OpO Base Int Integer
             deriving ( Generic )

instance Show Operand where
  show op = printf "%d = %s" (operandToInteger op) (gshowsPrec 0 op "")

operandToInteger :: Operand -> Integer
operandToInteger (OpI _ i) = i
operandToInteger (OpH _ h) = getHuge h
operandToInteger (OpL li) = luaIntToInteger li
operandToInteger (OpO (MkBase b) o i) = (b^o) * i

instance Eq Operand where
  a == b = operandToInteger a == operandToInteger b

instance Ord Operand where
  compare a b = compare (operandToInteger a) (operandToInteger b)

instance Num Operand where
  a + b = OpI Nothing $ operandToInteger a + operandToInteger b
  a - b = OpI Nothing $ max 0 (operandToInteger a - operandToInteger b)
  a * b = OpI Nothing $ operandToInteger a * operandToInteger b
  abs a = OpI Nothing $ abs $ operandToInteger a
  signum a = OpI Nothing $ signum $ operandToInteger a
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
  divMod a b =
    let (q, r) = operandToInteger a `divMod` operandToInteger b in
    (fromInteger q, fromInteger r)

instance Peekable Operand where
  safepeek idx = retrieving "operand" $ do
    n <- peekFieldRaw peekIntegral "n" idx
    as <- flip mapM [1..n] $ \i -> do
      peekIndexRaw i (\idx -> fromMaybe 0 <$> peekNilOr peekIntegral idx) idx
    o :: Integer <- peekFieldRaw peekIntegral "o" idx
    b <- peekFieldRaw peekIntegral "base" idx
    return $ OpI (Just . MkBase $ b) $ (* b^o) $ evalInBase b as

instance Pushable Operand where
  push (OpI Nothing i) = dostring' $ printf "return N.fromstring('%s')" (show i)
  push (OpI (Just (MkBase b)) i) = dostring' $ printf "return N.fromstring('%s', 10, %d)" (show i) b
  push o@(OpH Nothing _) = dostring' $ printf "return N.fromstring('%s')" (show $ operandToInteger o)
  push o@(OpH (Just (MkBase b)) _) = dostring' $ printf "return N.fromstring('%s', 10, %d)" (show $ operandToInteger o) b
  push (OpL (LuaInt li)) = pushinteger li
  push (OpO (MkBase b) o i) = ensureStackDiff 1 $ do
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
    where genBaseM = oneof [ return Nothing, Just <$> arbitrary ]
          genI = OpI <$> genBaseM <*> (getNonNegative <$> arbitrary)
          genH = OpH <$> genBaseM <*> arbitrary
          genL = OpL <$> getNonNegative <$> arbitrary
          genO = do
            b <- arbitrary
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

instance I.IsLuaNative Operand where
  isLuaNative (OpL _) = True
  isLuaNative _ = False

instance I.IntegerLike Operand where

spec :: Spec
spec = do
  it "should have the expected max_base" $ do
    b <- runLua $ return' "N.max_base"
    b `shouldBe` maxBase

  it "should have the expected default_base" $ do
    b <- runLua $ return' "N.default_base"
    b `shouldBe` maxBase

  it "should survive a push and peek roundtrip" $ properly $ I.mkProp I.relevantIfNotNative $ \(a :: Operand) -> do
      b <- runLua $ push a >> peek'
      b `shouldBe` a

  describe "integer-like" $ I.runSpec @Operand runLua $
    I.MkSpec { binary = [ I.add "N", I.sub "N"
                        , I.mul "N"
                        , I.quot "N", I.rem "N", I.quotrem "N"
                        , I.div "N", I.mod "N", I.divmod "N"
                        , I.compare "N"
                        ]
                     ++ I.relationalOperators "N"
             , unary = [ I.neg "N" ]
                    ++ [ I.abs "N", I.sign "N" ]
                    ++ [ I.tostring "N", I.fromstring "N"
                       , I.tointeger "N", I.frominteger "N"
                       , I.tohex "N", I.fromhex "N"
                       , I.tobigendian "N", I.frombigendian "N"
                       , I.tolittleendian "N", I.fromlittleendian "N"
                       ]
             }

  it "should refuse to convert negative integers" $ properly $ \(Negative (a :: LuaInt)) -> do
    Just (Exception msg) <- runLua $ do
      "a" `bind` a
      expectError' "N.frominteger(a)"
    msg `shouldEndWith` "unexpected negative integer"

  I.run2 runLua (I.tobase @Operand "N")
