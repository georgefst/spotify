module Spotify.Servant.Playlists where

import Spotify.Servant.Core
import Spotify.Types.Internal.CustomJSON
import Spotify.Types.Misc
import Spotify.Types.Simple qualified as Simple

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant.API (
    Capture,
    type (:>),
 )

type AddToPlaylist =
    "playlists"
        :> Capture "playlist_id" ID
        :> "tracks"
        :> SpotBody AddToPlaylistBody
        :> SpotPostCreated AddToPlaylistResponse
data AddToPlaylistBody = AddToPlaylistBody
    { position :: Maybe Int
    , uris :: [URI]
    }
    deriving (Generic)
    deriving (ToJSON)
newtype AddToPlaylistResponse = AddToPlaylistResponse
    { snapshotId :: ID
    }
    deriving (Generic)
    deriving (FromJSON) via CustomJSON AddToPlaylistResponse

type GetMyPlaylists =
    "me"
        :> "playlists"
        :> SpotPaging Simple.Playlist

type CreatePlaylist =
    "users"
        :> Capture "user_id" ID
        :> "playlists"
        :> SpotBody CreatePlaylistBody
        :> SpotPostCreated Simple.Playlist
data CreatePlaylistBody = CreatePlaylistBody
    { name :: Text
    , public :: Bool
    , collaborative :: Bool
    , description :: Text
    }
    deriving (Generic)
    deriving (ToJSON)
