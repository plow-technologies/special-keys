{-# LANGUAGE OverloadedStrings, TemplateHaskell, DeriveDataTypeable, TypeFamilies, GeneralizedNewtypeDeriving #-}
module Keys.UUID where

import Prelude
import Data.Bits
import Data.Char
import Data.UUID (fromString, toString, toWords, fromWords)
import qualified Data.UUID as U (UUID(..), nil)
import Data.UUID.V4(nextRandom)
import Control.Applicative ((<$>))
-- import Data.Aeson.Types (FromJSON(..), ToJSON(..), typeMismatch)
-- import qualified Data.Aeson.Types as Aeson (Value(String))
-- import Data.Text.Encoding (encodeUtf8, decodeUtf8)
-- import Control.Monad(mzero)
import Data.Data
import Data.Hashable
import Control.Monad
import Data.Text (unpack, pack, Text)
import qualified Data.Text as T
import Keys.Random
import Data.SafeCopy
import Data.Serialize
import Web.PathPieces
import Foreign.Storable
import Text.Blaze
import Data.Word
import Data.Aeson
import qualified Data.Aeson as Aeson

newtype UUID = UUID U.UUID
    deriving(Ord, Eq, Data, Typeable, Storable)

instance Show UUID where
    show = strUUID
instance Read UUID where
    readsPrec x y = map (\(a,b) -> (UUID a, b)) $ readsPrec x y

unUUID :: UUID -> U.UUID
unUUID (UUID u) = u

strUUID :: UUID -> String
strUUID (UUID u) = toString u

txtUUID :: UUID -> Text
txtUUID (UUID u) = uuidToText (toWords u) 
    where
        uuidToText :: (Word32, Word32, Word32, Word32) -> Text
        uuidToText (w0, w1, w2, w3) = hexw w0 $ hexw' w1 $ hexw' w2 $ hexw w3 T.empty
        hexw :: Word32 -> Text -> Text
        hexw  w s = hexn w 28 `T.cons` hexn w 24 `T.cons` hexn w 20 `T.cons` hexn w 16
                  `T.cons` hexn w 12 `T.cons` hexn w  8 `T.cons` hexn w  4 `T.cons` hexn w  0 `T.cons` s

        hexw' :: Word32 -> Text -> Text 
        hexw' w s = '-' `T.cons` hexn w 28 `T.cons` hexn w 24 `T.cons` hexn w 20 `T.cons` hexn w 16
                    `T.cons` '-' `T.cons` hexn w 12 `T.cons` hexn w  8 `T.cons` hexn w  4 `T.cons` hexn w  0 `T.cons` s

        hexn :: Word32 -> Int -> Char
        hexn w r = intToDigit $ fromIntegral ((w `shiftR` r) .&. 0xf)
        

nil :: UUID
nil = UUID U.nil

instance Serialize UUID where
    put (UUID u) = put $ toWords u
    get = (UUID . (\(w1,w2,w3,w4) -> fromWords w1 w2 w3 w4)) `fmap` get

instance HasRandom UUID where
    rnd = UUID `fmap` nextRandom

instance ToMarkup UUID where
    toMarkup u = toMarkup $ pack $ strUUID u

instance ToJSON UUID where
    toJSON u = Aeson.String (txtUUID u)
instance FromJSON UUID where
    parseJSON (Aeson.String s) = case fromString (T.unpack s) of
                                    Just u -> return $ UUID u
                                    Nothing -> mzero
    parseJSON _ = mzero

{- these can be useful in postgresql-simple:
instance FromField UUID where
    fromField f bs = 
        case bs of
            Nothing -> returnError UnexpectedNull f ""
            -- Parse the string values in the enum of sql/tableAuth.sql
            Just s -> case fromString . unpack . decodeUtf8 $ s of
                        Just uuid -> pure $ UUID $ uuid
                        Nothing -> returnError ConversionFailed f $ 
                            "UUID must be a 16 bytes long ByteString in network byte order. Input UUID was: " ++ 
                                (unpack (decodeUtf8 s))
instance ToField UUID where
    toField (UUID u) = Escape . encodeUtf8 . pack . toString $ u
    -}

instance Hashable UUID where
    hashWithSalt i (UUID u) = hashWithSalt i (toWords u)

instance SafeCopy UUID where
    putCopy (UUID u) = contain $ safePut $ toWords u
    getCopy = contain $ (UUID . (\(a,b,c,d) -> fromWords a b c d) <$> safeGet)

instance PathPiece UUID where
    -- fromPathPiece :: Text -> Maybe s
    fromPathPiece t = UUID <$> (fromString $ unpack t)
    -- toPathPiece :: s -> Text
    toPathPiece (UUID u) = pack $ toString u

