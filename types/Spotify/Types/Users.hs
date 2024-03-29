module Spotify.Types.Users where

import Spotify.Types.Internal.CustomJSON
import Spotify.Types.Misc

import Data.Aeson (FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data User = User
    { country :: Maybe Text
    , displayName :: Maybe Text
    , email :: Maybe Text
    , explicitContent :: Maybe ExplicitContent
    , externalUrls :: ExternalURLs
    , followers :: Maybe Followers
    , href :: Href
    , id :: UserID
    , images :: Maybe [Image]
    , product :: Maybe Product
    , uri :: URI
    }
    deriving (Eq, Ord, Show, Generic)
    deriving (FromJSON) via CustomJSON User
