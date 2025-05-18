module Spotify.Servant where

import Spotify.Servant.Albums
import Spotify.Servant.Artists
import Spotify.Servant.Categories
import Spotify.Servant.Core
import Spotify.Servant.Episodes
import Spotify.Servant.Player
import Spotify.Servant.Playlists
import Spotify.Servant.Search
import Spotify.Servant.Tracks
import Spotify.Servant.Users

import Servant.API ((:<|>), (:>))

type API =
    AuthHeader
        :> ( Albums
                :<|> Artists
                :<|> Categories
                :<|> Episodes
                :<|> Player
                :<|> Playlists
                :<|> Search
                :<|> Tracks
                :<|> Users
           )

type AccountsAPI =
    RefreshAccessToken
        :<|> RequestAccessToken
        :<|> Authorize
