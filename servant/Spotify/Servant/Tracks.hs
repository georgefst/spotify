module Spotify.Servant.Tracks where

import Spotify.Servant.Core
import Spotify.Types.Misc
import Spotify.Types.Tracks

import Servant.API (
    Capture,
    DeleteNoContent,
    Get,
    JSON,
    QueryParam,
    ReqBody,
    type (:>),
 )

type GetTrack =
    "tracks"
        :> Capture "id" TrackID
        :> QueryParam "market" Market
        :> Get '[JSON] Track

type GetSavedTracks =
    "me"
        :> "tracks"
        :> QueryParam "market" Market
        :> SpotPaging SavedTrack

type SaveTracks =
    "me"
        :> "tracks"
        :> ReqBody '[JSON] (SpotIDs TrackID)
        :> DeleteNoContent

type RemoveTracks =
    "me"
        :> "tracks"
        :> ReqBody '[JSON] (SpotIDs TrackID)
        :> DeleteNoContent
