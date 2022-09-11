module Spotify.Servant.Tracks where

import Spotify.Servant.Core
import Spotify.Types.Misc
import Spotify.Types.Tracks

import Servant.API (
    Capture,
    NoContent,
    QueryParam,
    type (:>),
 )

type GetTrack =
    "tracks"
        :> Capture "id" ID
        :> QueryParam "market" Market
        :> SpotGet Track

type GetSavedTracks =
    "me"
        :> "tracks"
        :> QueryParam "market" Market
        :> SpotPaging SavedTrack

type SaveTracks =
    "me"
        :> "tracks"
        :> SpotBody [ID]
        :> SpotDelete NoContent

type RemoveTracks =
    "me"
        :> "tracks"
        :> SpotBody [ID]
        :> SpotDelete NoContent
