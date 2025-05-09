module BigintSpec where

import Control.Monad ( when )
import Data.Maybe ( fromMaybe )
import Data.Ratio ( (%) )
import System.IO.Unsafe ( unsafePerformIO )
import System.Random ( randomIO )
import Text.Printf

import Test.Hspec
import Test.QuickCheck

import HsLua hiding ( Integer, compare )
import HsLua.Marshalling.Peekers

import Huge
import LuaUtils
import LuaBigInt
import qualified IntegerLike as I
import Utils

runLua :: RunLuaRun
runLua = mkRun $ do
  prepare
  "I" `requireG` "bigint"

type Base = Integer
type Sign = Integer

data Operand = OpI (Maybe Base) Integer
             | OpH (Maybe Base) Sign Huge
             -- | OpL LuaInt
             | OpO Base Int Integer
             deriving ( Show )
--instance Show Operand where
  --show = show . operandToInteger

operandToInteger :: Operand -> Integer
operandToInteger (OpI _ i) = i
operandToInteger (OpH _ sign h) = (* sign) $ getHuge h
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

instance Pushable Operand where
  push (OpI Nothing i) = dostring' $ printf "return I.fromstring('%s')" (show i)
  push (OpI (Just b) i) = dostring' $ printf "return I.fromstring('%s', 10, %d)" (show i) b
  push o@(OpH Nothing _ _) = dostring' $ printf "return I.fromstring('%s')" (show $ operandToInteger o)
  push o@(OpH (Just b) _ _) = dostring' $ printf "return I.fromstring('%s', 10, %d)" (show $ operandToInteger o) b
  push (OpO b o i) = ensureStackDiff 1 $ do
    dostring' "return I.make"

    let ds = digitsInBase b (abs i)
        n = length ds

    createtable n 4
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

    pushinteger $ fromIntegral (signum i)
    setfield t "sign"

    call 1 1

instance Peekable Operand where
  safepeek idx = retrieving "operand" $ do
    sign <- peekFieldRaw peekIntegral "sign" idx

    (b, i) <- cleanup $ do
      abs <- liftLua $ do
        _ <- getfield idx "abs"
        absindex top
      n <- peekFieldRaw peekIntegral "n" abs
      as <- flip mapM [1..n] $ \i -> do
        peekIndexRaw i (\idx -> fromMaybe 0 <$> peekNilOr peekIntegral idx) abs
      o :: Integer <- peekFieldRaw peekIntegral "o" abs
      b <- peekFieldRaw peekIntegral "base" abs
      return (b, (* sign) . (* b^o) $ evalInBase b as)

    return $ OpI (Just b) i

instance I.IsLuaNative Operand where
  isLuaNative _ = False

instance Arbitrary Operand where
  arbitrary = frequency [ (50, genI)
                        , (20, genH)
                        , (30, genO)
                        ]
    where genBase = chooseInteger (2, maxBase)
          genBaseM = oneof [ return Nothing, Just <$> genBase ]
          genI = OpI <$> genBaseM <*> arbitrary
          genH = OpH <$> genBaseM <*> elements [ 1, -1 ] <*> arbitrary
          genO = do
            b <- genBase
            o <- getNonNegative <$> arbitrary
            i <- operandToInteger <$> oneof [ genI ]
            return $ OpO b o i

  shrink (OpI mb i) = OpI mb <$> shrink i
  shrink (OpH mb sign h) = OpH mb sign <$> shrink h
  shrink (OpO b o i) = do
    o' <- shrink o
    i' <- shrink i
    b' <- filter (>= 2) $ shrink b
    return $ OpO b' o' i'

spec :: Spec
spec = do
  it "should load" $ do
    runLua @IO $ return ()

  it "should survive a push and peek roundtrip" $ properly $ I.unary $ \(a :: Operand) -> do
    b <- runLua $ push a >> peek'
    b `shouldBe` a

  describe "representations" $ do
    describe "decimal" $ do
      it "should render decimal strings" $ properly $ I.unary $ \(a :: Operand) -> do
        s <- runLua $ do
          "a" `bind` a
          return' "a:tostring()"
        s `shouldBe` (show $ toInteger a)

      it "should parse decimal strings" $ properly $ \(a :: Operand) -> do
        a' <- runLua $ return' $ printf "I.fromstring('%s')" (show $ toInteger a)
        a' `shouldBe` a

  I.integerLike @Operand runLua $ mempty
    <> I.add "I.add" <> I.sub "I.sub"
