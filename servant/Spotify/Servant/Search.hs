module Spotify.Servant.Search where

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
import Spotify.Servant.Core (SpotPaging')

type Search = GetSearch

type GetSearch =
    "search"
        :> QueryParam' '[Strict, Required] "q" Text
        :> QueryParam' '[Strict, Required] "type" [SearchType]
        :> QueryParam "include_external" Text
        :> QueryParam "market" Market
        :> SpotPaging' SearchResult
