module Spotify.Types.Simple where

import Spotify.Types.Internal.CustomJSON
import Spotify.Types.Misc

import Data.Aeson (FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data User = User
    { displayName :: Maybe Text
    , externalUrls :: ExternalURL
    , followers :: Maybe Followers
    , href :: Href
    , id :: ID
    , images :: Maybe [Image]
    , uri :: URI
    }
    deriving (Eq, Ord, Show, Generic)
    deriving (FromJSON) via CustomJSON User

data Track = Track
    { artists :: [Artist]
    , availableMarkets :: Maybe [Text]
    , discNumber :: Int
    , durationMs :: Int
    , explicit :: Bool
    , externalUrls :: ExternalURL
    , href :: Href
    , id :: ID
    , isPlayable :: Maybe Bool
    , linkedFrom :: Maybe TrackLink
    , name :: Text
    , previewUrl :: Maybe Text
    , trackNumber :: Int
    , uri :: URI
    }
    deriving (Eq, Ord, Show, Generic)
    deriving (FromJSON) via CustomJSON Track

data Album = Album
    { albumType :: AlbumType
    , artists :: [Artist]
    , availableMarkets :: Maybe [Text]
    , externalUrls :: ExternalURL
    , albumGroup :: Maybe AlbumGroup
    , href :: Href
    , id :: ID
    , images :: [Image]
    , name :: Text
    , releaseDate :: Text
    , releaseDatePrecision :: DatePrecision
    , restrictions :: Maybe Restrictions
    , uri :: URI
    }
    deriving (Eq, Ord, Show, Generic)
    deriving (FromJSON) via CustomJSON Album

data Artist = Artist
    { externalUrls :: ExternalURL
    , href :: Href
    , id :: ID
    , name :: Text
    , uri :: URI
    }
    deriving (Eq, Ord, Show, Generic)
    deriving (FromJSON) via CustomJSON Artist

data Playlist = Playlist
    { collaborative :: Bool
    , externalUrls :: ExternalURL
    , href :: Href
    , id :: ID
    , images :: [Image]
    , name :: Text
    , owner :: User
    , public :: Maybe Bool
    , snapshotId :: ID
    , tracks :: Tracks
    , uri :: URI
    }
    deriving (Eq, Ord, Show, Generic)
    deriving (FromJSON) via CustomJSON Playlist
