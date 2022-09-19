module Spotify.Types.Albums where

import Spotify.Types.Internal.CustomJSON
import Spotify.Types.Misc
import Spotify.Types.Simple

import Data.Aeson (FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data Album = Album
    { albumType :: AlbumType
    , artists :: [ArtistSimple]
    , availableMarkets :: Maybe [Text]
    , copyrights :: [Copyright]
    , externalIds :: ExternalIDs
    , externalUrls :: ExternalURLs
    , genres :: [Text]
    , href :: Href
    , id :: AlbumID
    , images :: [Image]
    , label :: Text
    , name :: Text
    , popularity :: Int
    , releaseDate :: Text
    , releaseDatePrecision :: DatePrecision
    , restrictions :: Maybe Restrictions
    , tracks :: Paging TrackSimple
    , uri :: URI
    }
    deriving (Eq, Ord, Show, Generic)
    deriving (FromJSON) via CustomJSON Album
