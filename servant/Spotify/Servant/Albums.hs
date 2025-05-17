module Spotify.Servant.Albums where

import Spotify.Servant.Core
import Spotify.Types.Albums
import Spotify.Types.Misc
import Spotify.Types.Simple

import Servant.API (
    Capture,
    DeleteNoContent,
    Get,
    JSON,
    QueryParam,
    ReqBody,
    type (:>),
 )

type GetAlbum =
    "albums"
        :> Capture "id" AlbumID
        :> QueryParam "market" Market
        :> Get '[JSON] Album

type GetAlbumTracks =
    "albums"
        :> Capture "id" AlbumID
        :> "tracks"
        :> QueryParam "market" Market
        :> SpotPaging TrackSimple

type RemoveAlbums =
    "me"
        :> "albums"
        :> ReqBody '[JSON] (SpotIDs AlbumID)
        :> DeleteNoContent
