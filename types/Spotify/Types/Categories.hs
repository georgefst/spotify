module Spotify.Types.Categories where

import Spotify.Types.Internal.CustomJSON
import Spotify.Types.Misc

import Data.Aeson (FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data Category = Category
    { href :: Href
    , icons :: [Image]
    , id :: ID
    , name :: Text
    }
    deriving (Eq, Ord, Show, Generic)
    deriving (FromJSON) via CustomJSON Category
