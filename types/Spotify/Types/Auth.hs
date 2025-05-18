module Spotify.Types.Auth where

import Spotify.Types.Internal.CustomJSON

import Control.Monad ((>=>))
import Data.Aeson (FromJSON, parseJSON)
import Data.ByteString.Base64 qualified as B64
import Data.HashMap.Strict qualified as HM
import Data.String (IsString)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import GHC.Generics (Generic)
import Servant.API (FromHttpApiData, ToHttpApiData (toUrlPiece))
import Spotify.Types.Misc (URL (..))
import Web.FormUrlEncoded (Form (Form), ToForm)
import Web.Internal.FormUrlEncoded (toForm)

newtype ClientId = ClientId {unwrap :: Text}
    deriving newtype (Eq, Ord, Show, IsString, ToHttpApiData)

newtype ClientSecret = ClientSecret {unwrap :: Text}
    deriving newtype (Eq, Ord, Show, IsString)

newtype AccessToken = AccessToken {unwrap :: Text}
    deriving newtype (Eq, Ord, Show, IsString)
    deriving newtype (FromJSON)
instance ToHttpApiData AccessToken where
    toUrlPiece (AccessToken t) = toUrlPiece $ "Bearer " <> t

newtype RefreshToken = RefreshToken {unwrap :: Text}
    deriving newtype (Eq, Ord, Show, IsString)
    deriving newtype (FromJSON)
newtype AuthCode = AuthCode {unwrap :: Text}
    deriving newtype (Eq, Ord, Show, IsString, FromHttpApiData)
    deriving newtype (FromJSON)

data IdAndSecret = IdAndSecret ClientId ClientSecret
instance ToHttpApiData IdAndSecret where
    toUrlPiece (IdAndSecret (ClientId i) (ClientSecret s)) =
        toUrlPiece . ("Basic " <>) . decodeUtf8 . B64.encode $ encodeUtf8 $ i <> ":" <> s

data TokenType
    = TokenTypeBearer
    deriving (Eq, Ord, Show, Generic)
instance FromJSON TokenType where
    parseJSON =
        parseJSON >=> \case
            "Bearer" -> pure TokenTypeBearer
            s -> fail $ "unknown type: " <> s

data TokenResponse = TokenResponse
    { accessToken :: AccessToken
    , tokenType :: TokenType
    , expiresIn :: Int
    , scope :: Text
    }
    deriving (Eq, Ord, Show, Generic)
    deriving (FromJSON) via CustomJSON TokenResponse

data TokenResponse' = TokenResponse'
    { accessToken :: AccessToken
    , tokenType :: TokenType
    , expiresIn :: Int
    , scope :: Text
    , refreshToken :: RefreshToken
    }
    deriving (Eq, Ord, Show, Generic)
    deriving (FromJSON) via CustomJSON TokenResponse'

data RequestAccessTokenForm = RequestAccessTokenForm AuthCode URL
instance ToForm RequestAccessTokenForm where
    toForm (RequestAccessTokenForm (AuthCode t) r) =
        Form $
            HM.fromList
                [ ("grant_type", ["authorization_code"])
                , ("code", [t])
                , ("redirect_uri", [r.unwrap])
                ]

newtype RefreshAccessTokenForm = RefreshAccessTokenForm RefreshToken
instance ToForm RefreshAccessTokenForm where
    toForm (RefreshAccessTokenForm (RefreshToken t)) =
        Form $
            HM.fromList
                [ ("grant_type", ["refresh_token"])
                , ("refresh_token", [t])
                ]
