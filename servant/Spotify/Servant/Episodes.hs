module Spotify.Servant.Episodes where

import Spotify.Servant.Core
import Spotify.Types.Episodes
import Spotify.Types.Misc

import Servant.API (
    Capture,
    DeleteNoContent,
    Get,
    JSON,
    PutNoContent,
    QueryParam,
    ReqBody,
    (:<|>),
    type (:>),
 )

type Episodes =
    GetEpisode
        :<|> GetSavedEpisodes
        :<|> SaveEpisodes
        :<|> RemoveEpisodes

type GetEpisode =
    "episodes"
        :> Capture "id" EpisodeID
        :> QueryParam "market" Market
        :> Get '[JSON] Episode

type GetSavedEpisodes =
    "me"
        :> "episodes"
        :> QueryParam "market" Market
        :> SpotPaging SavedEpisode

type SaveEpisodes =
    "me"
        :> "episodes"
        :> ReqBody '[JSON] (IDs EpisodeID)
        :> PutNoContent

type RemoveEpisodes =
    "me"
        :> "episodes"
        :> ReqBody '[JSON] (IDs EpisodeID)
        :> DeleteNoContent
