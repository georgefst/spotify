module Spotify.Servant.Artists where

import Spotify.Servant.Core
import Spotify.Types.Artists
import Spotify.Types.Misc

import Servant.API (
    Capture,
    type (:>),
 )

type GetArtist =
    "artists"
        :> Capture "id" ArtistID
        :> SpotGet Artist
