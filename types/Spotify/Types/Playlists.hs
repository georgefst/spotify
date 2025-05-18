module Spotify.Types.Playlists where

import Spotify.Types.Internal.CustomJSON
import Spotify.Types.Misc
import Spotify.Types.Simple
import Spotify.Types.Tracks

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data Playlist = Playlist
    { collaborative :: Bool
    , description :: Maybe Text
    , externalUrls :: ExternalURLs
    , followers :: Followers
    , href :: Href
    , id :: PlaylistID
    , images :: [Image]
    , name :: Text
    , owner :: UserSimple
    , public :: Maybe Bool
    , snapshotId :: SnapshotID
    , tracks :: Paging SavedTrack
    , uri :: URI
    }
    deriving (Eq, Ord, Show, Generic)
    deriving (FromJSON) via CustomJSON Playlist

data AddToPlaylistBody = AddToPlaylistBody
    { position :: Maybe Int
    , uris :: [URI]
    }
    deriving (Generic)
    deriving (ToJSON)

newtype AddToPlaylistResponse = AddToPlaylistResponse
    { snapshotId :: SnapshotID
    }
    deriving (Generic)
    deriving (FromJSON) via CustomJSON AddToPlaylistResponse

data CreatePlaylistOpts = CreatePlaylistOpts
    { name :: Text
    , public :: Bool
    , collaborative :: Bool
    , description :: Text
    }
    deriving (Generic)
    deriving (ToJSON)
