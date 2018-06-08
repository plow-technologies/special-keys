module Keys.Random where
import Prelude
import System.Random.MWC
import Control.Monad
import Control.Monad.Primitive
import Data.ByteString as B
import Data.Word (Word8)
import Data.Text.Encoding
import Data.Text
import Keys.Constraints

{- | Converts a Char to a Word8. Took from MissingH -}
c2w8 :: Char -> Word8
c2w8 = fromIntegral . fromEnum

------------------------------------------------------------------------------
charRangeStart :: Word8
charRangeStart = c2w8 'a'

------------------------------------------------------------------------------
charRangeEnd :: Word8
charRangeEnd = c2w8 'Z'

------------------------------------------------------------------------------
genString :: Gen (PrimState IO) -> IO B.ByteString
genString g = do
    randomLen <- uniformR (128 :: Int, 4096 :: Int) g
    str <- replicateM randomLen $ uniformR (charRangeStart, charRangeEnd) g
    return $ B.pack str

genInt :: Gen (PrimState IO) -> IO Int
genInt g = uniform g
genIntRange :: (Int, Int) -> Gen (PrimState IO) -> IO Int
genIntRange r g = uniformR r g

rndTxt :: IO Text
rndTxt = withSystemRandom genString >>= return . decodeUtf8

rndInt :: IO Int
rndInt = withSystemRandom genInt

rndIntRange :: (Int, Int) -> IO Int
rndIntRange r = withSystemRandom (genIntRange r)

class HasRandom a where
    rnd :: IO a

instance HasRandom Text where
    rnd = rndTxt

instance HasRandom B.ByteString where
    rnd = withSystemRandom genString

instance (HasRandom a) => HasRandom (Maybe a) where
    rnd = Just `fmap` rnd

instance HasRandom Text128 where rnd = txtConstraint `fmap` rndTxt
instance HasRandom Text256 where rnd = txtConstraint `fmap` rndTxt
instance HasRandom Text512 where rnd = txtConstraint `fmap` rndTxt
instance HasRandom Text1024 where rnd = txtConstraint `fmap` rndTxt
instance HasRandom Text2048 where rnd = txtConstraint `fmap` rndTxt
instance HasRandom Text4096 where rnd = txtConstraint `fmap` rndTxt

