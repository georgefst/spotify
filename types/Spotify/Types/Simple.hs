module Spotify.Types.Simple where

import Spotify.Types.Internal.CustomJSON
import Spotify.Types.Misc

import Data.Aeson (FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data UserSimple = UserSimple
    { displayName :: Maybe Text
    , externalUrls :: ExternalURLs
    , followers :: Maybe Followers
    , href :: Href
    , id :: UserID
    , images :: Maybe [Image]
    , uri :: URI
    }
    deriving (Eq, Ord, Show, Generic)
    deriving (FromJSON) via CustomJSON UserSimple

data TrackSimple = TrackSimple
    { artists :: [ArtistSimple]
    , availableMarkets :: Maybe [Text]
    , discNumber :: Int
    , durationMs :: Int
    , explicit :: Bool
    , externalUrls :: ExternalURLs
    , href :: Href
    , id :: TrackID
    , isPlayable :: Maybe Bool
    , linkedFrom :: Maybe TrackLink
    , name :: Text
    , previewUrl :: Maybe Text
    , trackNumber :: Int
    , uri :: URI
    }
    deriving (Eq, Ord, Show, Generic)
    deriving (FromJSON) via CustomJSON TrackSimple

data AlbumSimple = AlbumSimple
    { albumType :: AlbumType
    , artists :: [ArtistSimple]
    , availableMarkets :: Maybe [Text]
    , externalUrls :: ExternalURLs
    , albumGroup :: Maybe AlbumGroup
    , href :: Href
    , id :: AlbumID
    , images :: [Image]
    , name :: Text
    , releaseDate :: Text
    , releaseDatePrecision :: DatePrecision
    , restrictions :: Maybe Restrictions
    , uri :: URI
    }
    deriving (Eq, Ord, Show, Generic)
    deriving (FromJSON) via CustomJSON AlbumSimple

data ArtistSimple = ArtistSimple
    { externalUrls :: ExternalURLs
    , href :: Href
    , id :: ArtistID
    , name :: Text
    , uri :: URI
    }
    deriving (Eq, Ord, Show, Generic)
    deriving (FromJSON) via CustomJSON ArtistSimple

data PlaylistSimple = PlaylistSimple
    { collaborative :: Bool
    , externalUrls :: ExternalURLs
    , href :: Href
    , id :: PlaylistID
    , images :: [Image]
    , name :: Text
    , owner :: UserSimple
    , public :: Maybe Bool
    , snapshotId :: SnapshotID
    , tracks :: Tracks
    , uri :: URI
    }
    deriving (Eq, Ord, Show, Generic)
    deriving (FromJSON) via CustomJSON PlaylistSimple

data ShowSimple = ShowSimple
    { availableMarkets :: Maybe [Text]
    , copyrights :: [Copyright]
    , description :: Text
    , htmlDescription :: Text
    , explicit :: Bool
    , externalUrls :: ExternalURLs
    , href :: Href
    , id :: ShowID
    , images :: [Image]
    , isExternallyHosted :: Maybe Bool
    , languages :: [Text]
    , mediaType :: Text
    , name :: Text
    , publisher :: Text
    , uri :: URI
    , totalEpisodes :: Maybe Int
    }
    deriving (Eq, Ord, Show, Generic)
    deriving (FromJSON) via CustomJSON ShowSimple
