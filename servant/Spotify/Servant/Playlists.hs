module Spotify.Servant.Playlists where

import Spotify.Servant.Core
import Spotify.Types.Misc
import Spotify.Types.Playlists
import Spotify.Types.Simple

import Servant.API (
    Capture,
    Get,
    JSON,
    PostCreated,
    ReqBody,
    (:<|>),
    type (:>),
 )

type Playlists =
    GetPlaylist
        :<|> AddToPlaylist
        :<|> GetMyPlaylists
        :<|> CreatePlaylist

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
