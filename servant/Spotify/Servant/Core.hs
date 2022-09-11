{-# OPTIONS_GHC -Wno-orphans #-}

module Spotify.Servant.Core where

import Spotify.Types.Auth
import Spotify.Types.Misc

import Data.HashMap.Strict qualified as HM
import Servant.API (
    Delete,
    FormUrlEncoded,
    Get,
    Header',
    JSON,
    Post,
    PostCreated,
    Put,
    QueryParam,
    ReqBody,
    Required,
    Strict,
    type (:>),
 )
import Web.FormUrlEncoded (Form (Form), ToForm (toForm))

type RefreshAccessToken =
    "token"
        :> ReqBody '[FormUrlEncoded] RefreshToken
        :> Header' '[Strict, Required] "Authorization" IdAndSecret
        :> Post '[JSON] TokenResponse
instance ToForm RefreshToken where
    toForm (RefreshToken t) =
        Form $
            HM.fromList
                [ ("grant_type", ["refresh_token"])
                , ("refresh_token", [t])
                ]

type AuthHeader = Header' '[Strict, Required] "Authorization" AccessToken

-- various patterns which appear throughout the API
type SpotGet a = AuthHeader :> Get '[JSON] a
type SpotPut a = AuthHeader :> Put '[JSON] a
type SpotPost a = AuthHeader :> Post '[JSON] a
type SpotPostCreated a = AuthHeader :> PostCreated '[JSON] a
type SpotDelete a = AuthHeader :> Delete '[JSON] a
type SpotBody = ReqBody '[JSON]
type SpotPaging a =
    QueryParam "limit" Int
        :> QueryParam "offset" Int
        :> SpotGet (Paging a)
