module Spotify.Servant.Users where

import Spotify.Types.Misc
import Spotify.Types.Users

import Servant.API (
    Capture,
    DeleteNoContent,
    Get,
    JSON,
    type (:>),
 )

type GetMe =
    "me"
        :> Get '[JSON] User

type GetUser =
    "users"
        :> Capture "user_id" UserID
        :> Get '[JSON] User

type UnfollowPlaylist =
    "playlists"
        :> Capture "playlist_id" PlaylistID
        :> "followers"
        :> DeleteNoContent
