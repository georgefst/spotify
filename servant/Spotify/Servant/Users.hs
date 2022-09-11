module Spotify.Servant.Users where

import Spotify.Servant.Core
import Spotify.Types.Misc
import Spotify.Types.Users

import Servant.API (
    Capture,
    NoContent,
    type (:>),
 )

type GetMe =
    "me"
        :> SpotGet User

type GetUser =
    "users"
        :> Capture "user_id" UserID
        :> SpotGet User

type UnfollowPlaylist =
    "playlists"
        :> Capture "playlist_id" PlaylistID
        :> "followers"
        :> SpotDelete NoContent
