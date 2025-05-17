module Spotify.Servant.Playlists where

import Spotify.Servant.Core
import Spotify.Types.Internal.CustomJSON
import Spotify.Types.Misc
import Spotify.Types.Playlists
import Spotify.Types.Simple

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant.API (
    Capture,
    Get,
    JSON,
    PostCreated,
    ReqBody,
    type (:>),
 )

type GetPlaylist =
    "playlists"
        :> Capture "playlist_id" PlaylistID
        :> Get '[JSON] Playlist

type AddToPlaylist =
    "playlists"
        :> Capture "playlist_id" PlaylistID
        :> "tracks"
        :> ReqBody '[JSON] AddToPlaylistBody
        :> PostCreated '[JSON] AddToPlaylistResponse
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

type GetMyPlaylists =
    "me"
        :> "playlists"
        :> SpotPaging PlaylistSimple

type CreatePlaylist =
    "users"
        :> Capture "user_id" UserID
        :> "playlists"
        :> ReqBody '[JSON] CreatePlaylistOpts
        :> PostCreated '[JSON] PlaylistSimple
data CreatePlaylistOpts = CreatePlaylistOpts
    { name :: Text
    , public :: Bool
    , collaborative :: Bool
    , description :: Text
    }
    deriving (Generic)
    deriving (ToJSON)
