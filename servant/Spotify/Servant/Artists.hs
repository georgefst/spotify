module Spotify.Servant.Artists where

import Spotify.Types.Artists
import Spotify.Types.Misc

import Servant.API (
    Capture,
    Get,
    JSON,
    type (:>),
 )

type GetArtist =
    "artists"
        :> Capture "id" ArtistID
        :> Get '[JSON] Artist
