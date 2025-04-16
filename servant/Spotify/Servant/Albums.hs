module Spotify.Servant.Albums where

import Spotify.Servant.Core
import Spotify.Types.Albums
import Spotify.Types.Misc
import Spotify.Types.Simple

import Servant.API (
    Capture,
    QueryParam,
    type (:>),
 )

type GetAlbum =
    "albums"
        :> Capture "id" AlbumID
        :> QueryParam "market" Market
        :> SpotGet Album

type GetAlbumTracks =
    "albums"
        :> Capture "id" AlbumID
        :> "tracks"
        :> QueryParam "market" Market
        :> SpotPaging TrackSimple

type RemoveAlbums =
    "me"
        :> "albums"
        :> SpotBody (SpotIDs AlbumID)
        :> SpotDeleteNoContent
