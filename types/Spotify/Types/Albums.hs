module Spotify.Types.Albums where

import Spotify.Types.Internal.CustomJSON
import Spotify.Types.Misc
import Spotify.Types.Simple qualified as Simple

import Data.Aeson (FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data Album = Album
    { albumType :: AlbumType
    , artists :: [Simple.Artist]
    , availableMarkets :: Maybe [Text]
    , copyrights :: [Copyright]
    , externalIds :: ExternalID
    , externalUrls :: ExternalURL
    , genres :: [Text]
    , href :: Href
    , id :: ID
    , images :: [Image]
    , label :: Text
    , name :: Text
    , popularity :: Int
    , releaseDate :: Text
    , releaseDatePrecision :: DatePrecision
    , restrictions :: Maybe Restrictions
    , tracks :: Paging Simple.Track
    , uri :: URI
    }
    deriving (Eq, Ord, Show, Generic)
    deriving (FromJSON) via CustomJSON Album
