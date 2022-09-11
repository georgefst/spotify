{- HLINT ignore "Use newtype instead of data" -}

module Spotify.Types.Misc where

import Spotify.Types.Internal.CustomJSON
import Spotify.Types.Internal.EnumJSON

import Data.Aeson (FromJSON, ToJSON)
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant.API (ToHttpApiData)

data Copyright = Copyright
    { text :: Text
    , type_ :: CopyrightType
    }
    deriving (Eq, Ord, Show, Generic)
    deriving (FromJSON) via CustomJSON Copyright

data CopyrightType
    = C
    | P
    deriving (Eq, Ord, Show, Generic)
    deriving (FromJSON)

data Error = Error
    { status :: HTTPError
    , message :: Text
    }
    deriving (Eq, Ord, Show, Generic)
    deriving (FromJSON) via CustomJSON Error

data Followers = Followers
    { href :: Maybe Text
    , total :: Int
    }
    deriving (Eq, Ord, Show, Generic)
    deriving (FromJSON) via CustomJSON Followers

data Image = Image
    { height :: Maybe Int
    , url :: Text
    , width :: Maybe Int
    }
    deriving (Eq, Ord, Show, Generic)
    deriving (FromJSON) via CustomJSON Image

data Paging a = Paging
    { href :: Href
    , items :: [a]
    , limit :: Int
    , next :: Maybe Text
    , offset :: Int
    , previous :: Maybe Text
    , total :: Int
    }
    deriving (Eq, Ord, Show, Generic)
    deriving (FromJSON) via CustomJSON (Paging a)

data Tracks = Tracks
    { href :: Maybe Text
    , total :: Int
    }
    deriving (Eq, Ord, Show, Generic)
    deriving (FromJSON) via CustomJSON Tracks

data DatePrecision
    = DatePrecisionYear
    | DatePrecisionMonth
    | DatePrecisionDay
    deriving (Eq, Ord, Show, Generic)
    deriving (FromJSON) via CustomJSON DatePrecision

data Key
    = KeyC
    | KeyCSharp
    | KeyD
    | KeyDSharp
    | KeyE
    | KeyF
    | KeyFSharp
    | KeyG
    | KeyGSharp
    | KeyA
    | KeyASharp
    | KeyB
    deriving (Eq, Ord, Show, Generic, Enum)
    deriving (FromJSON) via EnumJSON Key

data TrackLink = TrackLink
    { externalUrls :: ExternalURL
    , href :: Href
    , id :: ID
    , url :: Text
    }
    deriving (Eq, Ord, Show, Generic)
    deriving (FromJSON) via CustomJSON TrackLink

data AlbumGroup
    = GroupAlbum
    | GroupSingle
    | GroupCompilation
    | AppearsOn
    deriving (Eq, Ord, Show, Generic)
    deriving (FromJSON) via CustomJSON AlbumGroup

data AlbumType
    = AlbumTypeAlbum
    | AlbumTypeSingle
    | AlbumTypeCompilation
    deriving (Eq, Ord, Show, Generic)
    deriving (FromJSON) via CustomJSON AlbumType

data ExplicitContent = ExplicitContent
    { filterEnabled :: Bool
    , filterLocked :: Bool
    }
    deriving (Eq, Ord, Show, Generic)
    deriving (FromJSON) via CustomJSON ExplicitContent

data Product
    = Premium
    | Free
    deriving (Eq, Ord, Show, Generic)
    deriving (FromJSON) via CustomJSON Product

newtype Market = Market Text
    deriving newtype (ToHttpApiData)

newtype Genre = Genre Text
    deriving (Show)
    deriving newtype (Eq, Ord, FromJSON)

newtype Href = Href Text
    deriving (Show)
    deriving newtype (Eq, Ord, FromJSON)

newtype ID = ID Text
    deriving (Show)
    deriving newtype (Eq, Ord, FromJSON, ToJSON, ToHttpApiData)

newtype URL = URL Text
    deriving (Show)
    deriving newtype (Eq, Ord, FromJSON)

newtype URI = URI Text
    deriving (Show)
    deriving newtype (Eq, Ord, FromJSON, ToJSON)

newtype Country = Country Text
    deriving (Show)
    deriving newtype (Eq, Ord, FromJSON, ToJSON, ToHttpApiData)

newtype Locale = Locale Text
    deriving (Show)
    deriving newtype (Eq, Ord, FromJSON, ToJSON, ToHttpApiData)

newtype HTTPError = HTTPError Int
    deriving (Show)
    deriving newtype (Eq, Ord, FromJSON)

newtype Restrictions = Restrictions (Map Text Text)
    deriving (Show)
    deriving newtype (Eq, Ord, FromJSON)

newtype ExternalID = ExternalID (Map Text Text)
    deriving (Show)
    deriving newtype (Eq, Ord, FromJSON)

newtype ExternalURL = ExternalURL (Map Text Text)
    deriving (Show)
    deriving newtype (Eq, Ord, FromJSON)
