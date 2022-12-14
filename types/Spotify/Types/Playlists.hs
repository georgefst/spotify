module Spotify.Types.Playlists where

import Spotify.Types.Internal.CustomJSON
import Spotify.Types.Misc
import Spotify.Types.Simple
import Spotify.Types.Tracks

import Data.Aeson (FromJSON)
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
