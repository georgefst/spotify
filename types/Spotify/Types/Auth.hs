module Spotify.Types.Auth where

import Spotify.Types.Internal.CustomJSON

import Control.Monad ((>=>))
import Data.Aeson (FromJSON, parseJSON)
import Data.ByteString.Base64 qualified as B64
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import GHC.Generics (Generic)
import Servant.API (ToHttpApiData (toUrlPiece))

newtype ClientId = ClientId Text
    deriving (Eq, Ord, Show)

newtype ClientSecret = ClientSecret Text
    deriving (Eq, Ord, Show)

newtype AccessToken = AccessToken Text
    deriving (Eq, Ord, Show)
    deriving newtype (FromJSON)
instance ToHttpApiData AccessToken where
    toUrlPiece (AccessToken t) = toUrlPiece $ T.pack "Bearer " <> t

newtype RefreshToken = RefreshToken Text
    deriving (Eq, Ord, Show)
    deriving newtype (FromJSON)

data IdAndSecret = IdAndSecret ClientId ClientSecret
instance ToHttpApiData IdAndSecret where
    toUrlPiece (IdAndSecret (ClientId i) (ClientSecret s)) =
        toUrlPiece . (T.pack "Basic " <>) . decodeUtf8 . B64.encode $ encodeUtf8 $ i <> T.pack ":" <> s

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
