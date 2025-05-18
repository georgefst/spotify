{-# LANGUAGE UndecidableInstances #-}

module Spotify.Types.Internal.CustomJSON where

import Data.Aeson (
    FromJSON (parseJSON),
    GFromJSON,
    GToJSON,
    Options (constructorTagModifier, fieldLabelModifier),
    ToJSON,
    Zero,
    defaultOptions,
    genericParseJSON,
    genericToJSON,
    toJSON,
 )
import Data.Char (isUpper, toLower)
import Data.List (dropWhileEnd, stripPrefix)
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic (Rep))
import Type.Reflection (Typeable, typeRep)

newtype CustomJSON a = CustomJSON a deriving (Generic)
instance (Generic a, GToJSON Zero (Rep a), Typeable a) => ToJSON (CustomJSON a) where
    toJSON = genericToJSON (opts @a) . \(CustomJSON x) -> x
instance (Generic a, GFromJSON Zero (Rep a), Typeable a) => FromJSON (CustomJSON a) where
    parseJSON = fmap CustomJSON . genericParseJSON (opts @a)
opts :: forall a. (Typeable a) => Options
opts = defaultOptions{constructorTagModifier, fieldLabelModifier}
      where
        fieldLabelModifier = camelToSnake . dropWhileEnd (== '_')
        constructorTagModifier = camelToSnake . (fromMaybe <*> stripPrefix (show $ typeRep @a))
        camelToSnake = \case
            [] -> []
            x : xs -> toLower x : go xs
          where
            go = \case
                [] -> []
                x : xs ->
                    if isUpper x
                        then '_' : toLower x : go xs
                        else x : go xs
