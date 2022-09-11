module Spotify.Servant.Albums where

import Spotify.Servant.Core
import Spotify.Types.Albums
import Spotify.Types.Misc
import Spotify.Types.Simple qualified as Simple

import Servant.API (
    Capture,
    NoContent,
    QueryParam,
    type (:>),
 )

type GetAlbum =
    "albums"
        :> Capture "id" ID
        :> QueryParam "market" Market
        :> SpotGet Album

type GetAlbumTracks =
    "albums"
        :> Capture "id" ID
        :> "tracks"
        :> QueryParam "market" Market
        :> SpotPaging Simple.Track

type RemoveAlbums =
    "me"
        :> "albums"
        :> SpotBody [ID]
        :> SpotDelete NoContent
