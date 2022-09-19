module Spotify.Servant.Core where

import Orphans.Servant.Lucid ()
import Spotify.Types.Auth
import Spotify.Types.Internal.CustomJSON
import Spotify.Types.Misc

import Data.Aeson (FromJSON)
import Data.HashMap.Strict qualified as HM
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Servant.API (
    Delete,
    FormUrlEncoded,
    Get,
    Header',
    JSON,
    Post,
    PostCreated,
    PostNoContent,
    Put,
    PutAccepted,
    PutNoContent,
    QueryParam,
    QueryParam',
    ReqBody,
    Required,
    Strict,
    ToHttpApiData (toUrlPiece),
    type (:>),
 )
import Servant.HTML.Lucid (HTML)
import Web.FormUrlEncoded (Form (Form), ToForm (toForm))

type Authorize =
    "authorize"
        :> QueryParam' '[Strict, Required] "client_id" ClientId
        :> QueryParam' '[Strict, Required] "response_type" Text
        :> QueryParam' '[Strict, Required] "redirect_uri" URL
        :> QueryParam "state" Text
        :> QueryParam "scope" ScopeSet
        :> QueryParam "show_dialog" Bool
        :> Get '[HTML] Text

type RequestAccessToken =
    "token"
        :> ReqBody '[FormUrlEncoded] RequestAccessTokenForm
        :> Header' '[Strict, Required] "Authorization" IdAndSecret
        :> Post '[JSON] TokenResponse'
data RequestAccessTokenForm = RequestAccessTokenForm AuthCode URL
instance ToForm RequestAccessTokenForm where
    toForm (RequestAccessTokenForm (AuthCode t) r) =
        Form $
            HM.fromList
                [ ("grant_type", ["authorization_code"])
                , ("code", [t])
                , ("redirect_uri", [r.unwrap])
                ]
data TokenResponse' = TokenResponse'
    { accessToken :: AccessToken
    , tokenType :: TokenType
    , expiresIn :: Int
    , scope :: Text
    , refreshToken :: RefreshToken
    }
    deriving (Eq, Ord, Show, Generic)
    deriving (FromJSON) via CustomJSON TokenResponse'

type RefreshAccessToken =
    "token"
        :> ReqBody '[FormUrlEncoded] RefreshAccessTokenForm
        :> Header' '[Strict, Required] "Authorization" IdAndSecret
        :> Post '[JSON] TokenResponse
newtype RefreshAccessTokenForm = RefreshAccessTokenForm RefreshToken
instance ToForm RefreshAccessTokenForm where
    toForm (RefreshAccessTokenForm (RefreshToken t)) =
        Form $
            HM.fromList
                [ ("grant_type", ["refresh_token"])
                , ("refresh_token", [t])
                ]

type AuthHeader = Header' '[Strict, Required] "Authorization" AccessToken

-- various patterns which appear throughout the API
type SpotGet a = AuthHeader :> Get '[JSON] a
type SpotPut a = AuthHeader :> Put '[JSON] a
type SpotPutAccepted a = AuthHeader :> PutAccepted '[JSON] a
type SpotPutNoContent = AuthHeader :> PutNoContent
type SpotPost a = AuthHeader :> Post '[JSON] a
type SpotPostCreated a = AuthHeader :> PostCreated '[JSON] a
type SpotPostNoContent = AuthHeader :> PostNoContent
type SpotDelete a = AuthHeader :> Delete '[JSON] a
type SpotBody = ReqBody '[JSON]
type SpotPaging a =
    QueryParam "limit" Int
        :> QueryParam "offset" Int
        :> SpotGet (Paging a)

-- types that only exist for the instances
newtype ScopeSet = ScopeSet {unwrap :: Set Scope}
instance ToHttpApiData ScopeSet where
    toUrlPiece = toUrlPiece . T.intercalate " " . map showScope . Set.toList . (.unwrap)
