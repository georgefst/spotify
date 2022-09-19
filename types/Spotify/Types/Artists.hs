module Spotify.Types.Artists where

import Spotify.Types.Internal.CustomJSON
import Spotify.Types.Misc

import Data.Aeson (FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data Artist = Artist
    { externalUrls :: ExternalURLs
    , followers :: Followers
    , genres :: [Genre]
    , href :: Href
    , id :: ArtistID
    , images :: [Image]
    , name :: Text
    , popularity :: Int
    , uri :: URI
    }
    deriving (Eq, Ord, Show, Generic)
    deriving (FromJSON) via CustomJSON Artist
