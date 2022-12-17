module Spotify.Servant.Search where

import Spotify.Servant.Core
import Spotify.Types.Misc
import Spotify.Types.Search

import Data.Text (Text)
import Servant.API (
    QueryParam,
    QueryParam',
    Required,
    Strict,
    type (:>),
 )

type GetSearch =
    "search"
        :> QueryParam' '[Strict, Required] "q" Text
        :> QueryParam' '[Strict, Required] "type" [SearchType]
        :> QueryParam "include_external" Text
        :> QueryParam "limit" Int
        :> QueryParam "market" Market
        :> QueryParam "offset" Int
        :> SpotGet SearchResult
