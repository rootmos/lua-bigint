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
--import qualified IntegerLike as I
import qualified IntegerLike2 as I2
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
             deriving ( Generic )

instance Show Operand where
  show op = printf "%d = %s" (operandToInteger op) (gshowsPrec 0 op "")

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
    return $ OpI (Just b) $ (* b^o) $ evalInBase b as

instance Pushable Operand where
  push (OpI Nothing i) = dostring' $ printf "return N.fromstring('%s')" (show i)
  push (OpI (Just b) i) = dostring' $ printf "return N.fromstring('%s', 10, %d)" (show i) b
  push o@(OpH Nothing _) = dostring' $ printf "return N.fromstring('%s')" (show $ operandToInteger o)
  push o@(OpH (Just b) _) = dostring' $ printf "return N.fromstring('%s', 10, %d)" (show $ operandToInteger o) b
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

instance I2.IsLuaNative Operand where
  isLuaNative (OpL _) = True
  isLuaNative _ = False

-- TODO should this really be necessary?
instance I2.IntegerLike Operand where

spec :: Spec
spec = do
  it "should have the expected max_base" $ do
    b <- runLua $ return' "N.max_base"
    b `shouldBe` maxBase

  it "should have the expected default_base" $ do
    b <- runLua $ return' "N.default_base"
    b `shouldBe` maxBase

  it "should survive a push and peek roundtrip" $ properly $ I2.mkProp I2.relevantIfNotNative $ \(a :: Operand) -> do
      b <- runLua $ push a >> peek'
      b `shouldBe` a

  I2.integerLike @Operand runLua $
    I2.MkSpec { binary = [ I2.add "N", I2.sub "N"
                         , I2.mul "N"
                         , I2.quot "N", I2.rem "N", I2.quotrem "N"
                         , I2.div "N", I2.mod "N", I2.divmod "N"
                         , I2.compare "N"
                         ]
                      ++ I2.relationalOperators "N"
              , unary = [ I2.neg "N" ]
                     ++ [ I2.tostring "N", I2.fromstring "N"
                        , I2.tointeger "N", I2.frominteger "N"
                        ]
              }

  -- I.integerLike @Operand runLua $ mempty
    -- <> I.relationalOperators
    -- <> I.compare "N"
    -- <> I.add "N" <> I.mul "N"
    -- <> I.truncatingSubtraction "N"
    -- <> I.divrem "N"

  --describe "integer conversion" $ do
    --it "should convert from non-negative integers" $ properly $ \(NonNegative a) -> do
      --a' <- runLua $ do
        --"a" `bind` a
        --return' "N.frominteger(a)"
      --a' `shouldBe` OpL a

    --it "should refuse to convert negative integers" $ properly $ \(Negative (a :: LuaInt)) -> do
      --Just (Exception msg) <- runLua $ do
        --"a" `bind` a
        --expectError (dostring "N.frominteger(a)")
      --msg `shouldEndWith` "unexpected negative integer"

    --it "should safely try converting to native integers" $ properly $ I.unary $ \(a :: Operand) -> do
      --a' <- runLua $ do
        --"a" `bind` a
        --dostring' "return a:tointeger()"
        --isnil top >>= \case
          --True -> return Nothing
          --False -> Just <$> peek' @Integer
      --a' `shouldBe` (if a <= maxint then Just (toInteger a) else Nothing)

  --describe "representations" $ do
    --describe "decimal" $ do
      --it "should render decimal strings" $ properly $ I.unary $ \(a :: Operand) -> do
        --s <- runLua $ do
          --"a" `bind` a
          --return' "a:tostring()"
        --s `shouldBe` (show $ toInteger a)

      --it "should parse decimal strings" $ properly $ \(a :: Operand) -> do
        --a' <- runLua $ return' $ printf "N.fromstring('%s')" (show $ toInteger a)
        --a' `shouldBe` a

    --describe "hexadecimal" $ do
      --it "should render hexadecimal strings" $ properly $ I.unary $ \(a :: Operand) -> do
        --s <- runLua $ do
          --"a" `bind` a
          --return' "a:tohex()"
        --s `shouldBe` (toHex $ toInteger a)

      --it "should parse hexadecimal strings" $ properly $ \(a :: Operand) -> do
        --a' <- runLua $ return' $ printf "N.fromhex('%s')" (toHex $ toInteger a)
        --a' `shouldBe` a

    --describe "big-endian" $ do
      --it "should render big-endian bytestrings" $ properly $ I.unary $ \(a :: Operand) -> do
        --s <- runLua $ do
          --"a" `bind` a
          --return' "a:tobigendian()"
        --s `shouldBe` (toBeBytes $ toInteger a)

      --it "should parse big-endian bytestrings" $ properly $ \(a :: Operand) -> do
        --a' <- runLua $ do
          --"bs" `bind` (toBeBytes $ toInteger a)
          --return' "N.frombigendian(bs)"
        --a' `shouldBe` a

    --describe "little-endian" $ do
      --it "should render little-endian bytestrings" $ properly $ I.unary $ \(a :: Operand) -> do
        --s <- runLua $ do
          --"a" `bind` a
          --return' "a:tolittleendian()"
        --s `shouldBe` (toLeBytes $ toInteger a)

      --it "should parse little-endian bytestrings" $ properly $ \(a :: Operand) -> do
        --a' <- runLua $ do
          --"bs" `bind` (toLeBytes $ toInteger a)
          --return' "N.fromlittleendian(bs)"
        --a' `shouldBe` a
