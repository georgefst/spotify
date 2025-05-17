module Spotify.Servant.Categories where

import Spotify.Types.Categories
import Spotify.Types.Misc

import Servant.API (
    Capture,
    Get,
    JSON,
    QueryParam,
    type (:>),
 )

type GetCategories =
    "browse"
        :> "categories"
        :> Capture "category_id" CategoryID
        :> QueryParam "country" Country
        :> QueryParam "locale" Locale
        :> Get '[JSON] Category
