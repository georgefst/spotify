module Spotify.Servant.Episodes where

import Spotify.Servant.Core
import Spotify.Types.Episodes
import Spotify.Types.Misc

import Servant.API (
    Capture,
    QueryParam,
    type (:>),
 )

type GetEpisode =
    "episodes"
        :> Capture "id" EpisodeID
        :> QueryParam "market" Market
        :> SpotGet Episode

type GetSavedEpisodes =
    "me"
        :> "episodes"
        :> QueryParam "market" Market
        :> SpotPaging SavedEpisode

type SaveEpisodes =
    "me"
        :> "episodes"
        :> SpotBody (SpotIDs EpisodeID)
        :> SpotPutNoContent

type RemoveEpisodes =
    "me"
        :> "episodes"
        :> SpotBody (SpotIDs EpisodeID)
        :> SpotDeleteNoContent
