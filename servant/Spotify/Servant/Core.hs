module Spotify.Servant.Core where

import Orphans.Servant.Lucid ()
import Spotify.Types.Auth
import Spotify.Types.Misc

import Data.Aeson (FromJSON)
import Data.Aeson.Types (FromJSON (parseJSON))
import Data.Data (Proxy (Proxy))
import Data.Text (Text)
import Servant.API (
    FormUrlEncoded,
    Get,
    Header',
    JSON,
    MimeUnrender,
    OctetStream,
    Post,
    QueryParam,
    QueryParam',
    ReqBody,
    Required,
    StdMethod (GET),
    Strict,
    UVerb,
    Union,
    WithStatus (WithStatus),
    mimeUnrender,
    type (:>),
 )
import Servant.API.UVerb (foldMapUnion)
import Servant.HTML.Lucid (HTML)
import Spotify.Types.Player

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

type RefreshAccessToken =
    "token"
        :> ReqBody '[FormUrlEncoded] RefreshAccessTokenForm
        :> Header' '[Strict, Required] "Authorization" IdAndSecret
        :> Post '[JSON] TokenResponse

type AuthHeader = Header' '[Strict, Required] "Authorization" AccessToken

type SpotPaging a =
    QueryParam "limit" Int
        :> QueryParam "offset" Int
        :> Get '[JSON] (Paging a)

-- TODO this is all a rather elaborate workaround for limitations of `UVerb`
-- namely that the content-type can't depend on the return code
-- we'd like to just do something like:
-- `type SpotGetOrNoContent a = AuthHeader :> UVerb GET '[JSON, NoContent] '[WithStatus 200 a, WithStatus 204 NoContent]`
-- it's likely we could come up with a more opaque abstraction...
type SpotGetOrNoContent a = UVerb GET '[JSON, OctetStream] '[WithStatus 200 (OctetStreamInstanceWrapper a), WithStatus 204 NoContent']
data NoContent' = NoContent' deriving (Show)
instance FromJSON NoContent' where
    parseJSON _ = fail "dummy instance for SpotGetOrNoContent"
newtype OctetStreamInstanceWrapper a = OctetStreamInstanceWrapper a
    deriving newtype (FromJSON)
instance MimeUnrender OctetStream (OctetStreamInstanceWrapper PlaybackState) where
    mimeUnrender Proxy _ = Left "dummy instance for SpotGetOrNoContent"
instance MimeUnrender OctetStream NoContent' where
    mimeUnrender Proxy = \case
        "" -> Right NoContent'
        _ -> Left "unexpected content"
class HandleJSONOrNoContent a b where
    handleJSONOrNoContent :: b -> Maybe a
instance HandleJSONOrNoContent a (WithStatus 200 (OctetStreamInstanceWrapper a)) where
    handleJSONOrNoContent (WithStatus (OctetStreamInstanceWrapper b)) = Just b
instance HandleJSONOrNoContent a (WithStatus 204 NoContent') where
    handleJSONOrNoContent (WithStatus NoContent') = Nothing
handleAllJSONOrNoContent :: forall a. Union '[WithStatus 200 (OctetStreamInstanceWrapper a), WithStatus 204 NoContent'] -> Maybe a
handleAllJSONOrNoContent = foldMapUnion (Proxy @(HandleJSONOrNoContent a)) handleJSONOrNoContent
