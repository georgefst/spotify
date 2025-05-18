module Spotify.Servant.Search where

import Spotify.Types.Misc
import Spotify.Types.Search

import Data.Text (Text)
import Servant.API (
    Get,
    JSON,
    QueryParam,
    QueryParam',
    Required,
    Strict,
    type (:>),
 )

type Search = GetSearch

type GetSearch =
    "search"
        :> QueryParam' '[Strict, Required] "q" Text
        :> QueryParam' '[Strict, Required] "type" [SearchType]
        :> QueryParam "include_external" Text
        :> QueryParam "limit" Int
        :> QueryParam "market" Market
        :> QueryParam "offset" Int
        :> Get '[JSON] SearchResult
