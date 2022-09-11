module Spotify.Servant.Categories where

import Spotify.Servant.Core
import Spotify.Types.Categories
import Spotify.Types.Misc

import Servant.API (
    Capture,
    QueryParam,
    type (:>),
 )

type GetCategories =
    "browse"
        :> "categories"
        :> Capture "category_id" ID
        :> QueryParam "country" Country
        :> QueryParam "locale" Locale
        :> SpotGet Category
