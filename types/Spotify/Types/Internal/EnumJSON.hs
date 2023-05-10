module Spotify.Types.Internal.EnumJSON where

import Data.Aeson (FromJSON (parseJSON))

newtype EnumJSON a = EnumJSON a

instance (Enum a) => FromJSON (EnumJSON a) where
    parseJSON = fmap (EnumJSON . toEnum @a) . parseJSON
