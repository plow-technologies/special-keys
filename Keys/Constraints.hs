{-# LANGUAGE RankNTypes, DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
module Keys.Constraints
    ( Text128
    , Text256
    , Text512
    , Text1024
    , Text2048
    , Text4096
    , TextConstraint(..)
    , t128
    , t256
    , t512
    , t1024
    , t2048
    , t4096) where
import Prelude
import qualified Data.Text as T
import Data.Data
import Data.String
import Data.Monoid
import Control.Monad
import Control.DeepSeq
import Data.SafeCopy
import Data.Serialize
import Data.Text.Encoding
import Data.Hashable
import Data.Aeson
import qualified Data.Aeson as Aeson

-- | Text with a maximum of 128 characters
newtype Text128 = Text128 T.Text deriving(Eq, Data, Ord, Read, Show, Typeable, IsString, Monoid, NFData)
newtype Text256 = Text256 T.Text deriving(Eq, Data, Ord, Read, Show, Typeable, IsString, Monoid, NFData)
newtype Text512 = Text512 T.Text deriving(Eq, Data, Ord, Read, Show, Typeable, IsString, Monoid, NFData)
newtype Text1024 = Text1024 T.Text deriving(Eq, Data, Ord, Read, Show, Typeable, IsString, Monoid, NFData)
newtype Text2048 = Text2048 T.Text deriving(Eq, Data, Ord, Read, Show, Typeable, IsString, Monoid, NFData)
newtype Text4096 = Text4096 T.Text deriving(Eq, Data, Ord, Read, Show, Typeable, IsString, Monoid, NFData)

class TextConstraint a where
    txtConstraint :: T.Text -> a
    getTxt :: a -> T.Text

instance TextConstraint Text128 where txtConstraint = t128
                                      getTxt (Text128 t) = t
instance TextConstraint Text256 where txtConstraint = t256
                                      getTxt (Text256 t) = t
instance TextConstraint Text512 where txtConstraint = t512
                                      getTxt (Text512 t) = t
instance TextConstraint Text1024 where txtConstraint = t1024
                                       getTxt (Text1024 t) = t
instance TextConstraint Text2048 where txtConstraint = t2048
                                       getTxt (Text2048 t) = t
instance TextConstraint Text4096 where txtConstraint = t4096
                                       getTxt (Text4096 t) = t

instance (TextConstraint a) => TextConstraint (Maybe a) where
    txtConstraint t = Just $ txtConstraint t
    getTxt t = case t of
                 Just a -> getTxt a
                 Nothing -> T.empty

instance Hashable Text128 where hashWithSalt s d = hashWithSalt s (getTxt d)
instance Hashable Text256 where hashWithSalt s d = hashWithSalt s (getTxt d)
instance Hashable Text512 where hashWithSalt s d = hashWithSalt s (getTxt d)
instance Hashable Text1024 where hashWithSalt s d = hashWithSalt s (getTxt d)
instance Hashable Text2048 where hashWithSalt s d = hashWithSalt s (getTxt d)
instance Hashable Text4096 where hashWithSalt s d = hashWithSalt s (getTxt d)

instance Serialize Text128 where put (Text128 t) = put $ encodeUtf8 t
                                 get = (Text128 . decodeUtf8) `fmap` get
instance Serialize Text256 where put (Text256 t) = put $ encodeUtf8 t
                                 get = (Text256 . decodeUtf8) `fmap` get
instance Serialize Text512 where put (Text512 t) = put $ encodeUtf8 t
                                 get = (Text512 . decodeUtf8) `fmap` get
instance Serialize Text1024 where put (Text1024 t) = put $ encodeUtf8 t
                                  get = (Text1024 . decodeUtf8) `fmap` get
instance Serialize Text2048 where put (Text2048 t) = put $ encodeUtf8 t
                                  get = (Text2048 . decodeUtf8) `fmap` get
instance Serialize Text4096 where put (Text4096 t) = put $ encodeUtf8 t
                                  get = (Text4096 . decodeUtf8) `fmap` get

instance SafeCopy Text128 where putCopy (Text128 t) = putCopy t
                                getCopy = contain $ Text128 `fmap` safeGet
instance SafeCopy Text256 where putCopy (Text256 t) = putCopy t
                                getCopy = contain $ Text256 `fmap` safeGet
instance SafeCopy Text512 where putCopy (Text512 t) = putCopy t
                                getCopy = contain $ Text512 `fmap` safeGet
instance SafeCopy Text1024 where putCopy (Text1024 t) = putCopy t
                                 getCopy = contain $ Text1024 `fmap` safeGet
instance SafeCopy Text2048 where putCopy (Text2048 t) = putCopy t
                                 getCopy = contain $ Text2048 `fmap` safeGet
instance SafeCopy Text4096 where putCopy (Text4096 t) = putCopy t
                                 getCopy = contain $ Text4096 `fmap` safeGet

instance FromJSON Text128 where parseJSON (Aeson.String s) = return $ t128 s
                                parseJSON _ = mzero
instance FromJSON Text256 where parseJSON (Aeson.String s) = return $ t256 s
                                parseJSON _ = mzero
instance FromJSON Text512 where parseJSON (Aeson.String s) = return $ t512 s
                                parseJSON _ = mzero
instance FromJSON Text1024 where parseJSON (Aeson.String s) = return $ t1024 s
                                 parseJSON _ = mzero
instance FromJSON Text2048 where parseJSON (Aeson.String s) = return $ t2048 s
                                 parseJSON _ = mzero
instance FromJSON Text4096 where parseJSON (Aeson.String s) = return $ t4096 s
                                 parseJSON _ = mzero
instance ToJSON Text128 where toJSON s = Aeson.String (getTxt s)
instance ToJSON Text256 where toJSON s = Aeson.String (getTxt s)
instance ToJSON Text512 where toJSON s = Aeson.String (getTxt s)
instance ToJSON Text1024 where toJSON s = Aeson.String (getTxt s)
instance ToJSON Text2048 where toJSON s = Aeson.String (getTxt s)
instance ToJSON Text4096 where toJSON s = Aeson.String (getTxt s)

t128 :: T.Text -> Text128
t128 = Text128 . T.take 128

t256 :: T.Text -> Text256
t256 = Text256 . T.take 256

t512 :: T.Text -> Text512
t512 = Text512 . T.take 512 

t1024 :: T.Text -> Text1024
t1024 = Text1024 . T.take 1024

t2048 :: T.Text -> Text2048
t2048 = Text2048 . T.take 2048

t4096 :: T.Text -> Text4096
t4096 = Text4096 . T.take 4096

