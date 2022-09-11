{- HLINT ignore "Use newtype instead of data" -}
{-# LANGUAGE UndecidableInstances #-}

module Spotify.Types.Misc where

import Spotify.Types.Internal.CustomJSON
import Spotify.Types.Internal.EnumJSON

import Data.Aeson (FromJSON, ToJSON)
import Data.Map (Map)
import Data.Proxy (Proxy (Proxy))
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import GHC.Records (HasField)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
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
    , id :: TrackID
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

newtype Market = Market {unwrap :: Text}
    deriving newtype (Eq, Ord, ToHttpApiData, IsString)

newtype Genre = Genre {unwrap :: Text}
    deriving (Show)
    deriving newtype (Eq, Ord, FromJSON, IsString)

newtype Href = Href {unwrap :: Text}
    deriving (Show)
    deriving newtype (Eq, Ord, FromJSON, IsString)

class ToURI a where
    toURI :: a -> URI
newtype URIPrefix (s :: Symbol) a = URIPrefix a
instance (KnownSymbol s, HasField "unwrap" a Text) => ToURI (URIPrefix s a) where
    toURI (URIPrefix x) = URI $ "spotify:" <> T.pack (symbolVal (Proxy @s)) <> ":" <> x.unwrap
newtype AlbumID = AlbumID {unwrap :: Text}
    deriving (Show)
    deriving newtype (Eq, Ord, FromJSON, ToJSON, ToHttpApiData, IsString)
    deriving (ToURI) via URIPrefix "album" AlbumID
newtype ArtistID = ArtistID {unwrap :: Text}
    deriving (Show)
    deriving newtype (Eq, Ord, FromJSON, ToJSON, ToHttpApiData, IsString)
    deriving (ToURI) via URIPrefix "artist" ArtistID
newtype EpisodeID = EpisodeID {unwrap :: Text}
    deriving (Show)
    deriving newtype (Eq, Ord, FromJSON, ToJSON, ToHttpApiData, IsString)
    deriving (ToURI) via URIPrefix "episode" EpisodeID
newtype TrackID = TrackID {unwrap :: Text}
    deriving (Show)
    deriving newtype (Eq, Ord, FromJSON, ToJSON, ToHttpApiData, IsString)
    deriving (ToURI) via URIPrefix "track" TrackID
newtype UserID = UserID {unwrap :: Text}
    deriving (Show)
    deriving newtype (Eq, Ord, FromJSON, ToJSON, ToHttpApiData, IsString)
    deriving (ToURI) via URIPrefix "user" UserID
newtype PlaylistID = PlaylistID {unwrap :: Text}
    deriving (Show)
    deriving newtype (Eq, Ord, FromJSON, ToJSON, ToHttpApiData, IsString)
    deriving (ToURI) via URIPrefix "playlist" PlaylistID
newtype CategoryID = CategoryID {unwrap :: Text}
    deriving (Show)
    deriving newtype (Eq, Ord, FromJSON, ToJSON, ToHttpApiData, IsString)
newtype SnapshotID = SnapshotID {unwrap :: Text}
    deriving (Show)
    deriving newtype (Eq, Ord, FromJSON, ToJSON, ToHttpApiData, IsString)

newtype URL = URL {unwrap :: Text}
    deriving (Show)
    deriving newtype (Eq, Ord, FromJSON, IsString)

newtype URI = URI {unwrap :: Text}
    deriving (Show)
    deriving newtype (Eq, Ord, FromJSON, ToJSON, IsString)

newtype Country = Country {unwrap :: Text}
    deriving (Show)
    deriving newtype (Eq, Ord, FromJSON, ToJSON, ToHttpApiData, IsString)

newtype Locale = Locale {unwrap :: Text}
    deriving (Show)
    deriving newtype (Eq, Ord, FromJSON, ToJSON, ToHttpApiData, IsString)

newtype HTTPError = HTTPError {unwrap :: Int}
    deriving (Show)
    deriving newtype (Eq, Ord, FromJSON)

newtype Restrictions = Restrictions {unwrap :: Map Text Text}
    deriving (Show)
    deriving newtype (Eq, Ord, FromJSON)

newtype ExternalID = ExternalID {unwrap :: Map Text Text}
    deriving (Show)
    deriving newtype (Eq, Ord, FromJSON)

newtype ExternalURL = ExternalURL {unwrap :: Map Text Text}
    deriving (Show)
    deriving newtype (Eq, Ord, FromJSON)
