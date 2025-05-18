{- HLINT ignore "Use newtype instead of data" -}
{-# LANGUAGE UndecidableInstances #-}

module Spotify.Types.Misc where

import Spotify.Types.Internal.CustomJSON
import Spotify.Types.Internal.EnumJSON

import Data.Aeson (FromJSON, ToJSON)
import Data.List.Extra (enumerate)
import Data.Map (Map)
import Data.Proxy (Proxy (Proxy))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import GHC.Records (HasField)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Servant.API (ToHttpApiData, toUrlPiece)

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

data ResumePoint = ResumePoint
    { fullyPlayed :: Bool
    , resumePositionMs :: Int
    }
    deriving (Eq, Ord, Show, Generic)
    deriving (FromJSON) via CustomJSON ResumePoint

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
    { externalUrls :: ExternalURLs
    , href :: Href
    , id :: TrackID
    , url :: Maybe Text
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

data Offset = Offset
    { position :: Int
    , uri :: Maybe URI
    }
    deriving (Eq, Ord, Show, Generic)
    deriving (ToJSON)

newtype Market = Market {unwrap :: Text}
    deriving newtype (Eq, Ord, Show, ToHttpApiData, IsString)

newtype Genre = Genre {unwrap :: Text}
    deriving newtype (Eq, Ord, Show, FromJSON, IsString)

newtype Href = Href {unwrap :: Text}
    deriving newtype (Eq, Ord, Show, FromJSON, IsString)

class ToURI a where
    toURI :: a -> URI
newtype URIPrefix (s :: Symbol) a = URIPrefix a
instance (KnownSymbol s, HasField "unwrap" a Text) => ToURI (URIPrefix s a) where
    toURI (URIPrefix x) = URI $ "spotify:" <> T.pack (symbolVal (Proxy @s)) <> ":" <> x.unwrap
newtype DeviceID = DeviceID {unwrap :: Text}
    deriving newtype (Eq, Ord, Show, FromJSON, ToJSON, ToHttpApiData, IsString)
newtype AlbumID = AlbumID {unwrap :: Text}
    deriving newtype (Eq, Ord, Show, FromJSON, ToJSON, ToHttpApiData, IsString)
    deriving (ToURI) via URIPrefix "album" AlbumID
newtype ArtistID = ArtistID {unwrap :: Text}
    deriving newtype (Eq, Ord, Show, FromJSON, ToJSON, ToHttpApiData, IsString)
    deriving (ToURI) via URIPrefix "artist" ArtistID
newtype EpisodeID = EpisodeID {unwrap :: Text}
    deriving newtype (Eq, Ord, Show, FromJSON, ToJSON, ToHttpApiData, IsString)
    deriving (ToURI) via URIPrefix "episode" EpisodeID
newtype ShowID = ShowID {unwrap :: Text}
    deriving newtype (Eq, Ord, Show, FromJSON, ToJSON, ToHttpApiData, IsString)
    deriving (ToURI) via URIPrefix "show" ShowID
newtype TrackID = TrackID {unwrap :: Text}
    deriving newtype (Eq, Ord, Show, FromJSON, ToJSON, ToHttpApiData, IsString)
    deriving (ToURI) via URIPrefix "track" TrackID
newtype UserID = UserID {unwrap :: Text}
    deriving newtype (Eq, Ord, Show, FromJSON, ToJSON, ToHttpApiData, IsString)
    deriving (ToURI) via URIPrefix "user" UserID
newtype PlaylistID = PlaylistID {unwrap :: Text}
    deriving newtype (Eq, Ord, Show, FromJSON, ToJSON, ToHttpApiData, IsString)
    deriving (ToURI) via URIPrefix "playlist" PlaylistID
newtype CategoryID = CategoryID {unwrap :: Text}
    deriving newtype (Eq, Ord, Show, FromJSON, ToJSON, ToHttpApiData, IsString)
newtype SnapshotID = SnapshotID {unwrap :: Text}
    deriving newtype (Eq, Ord, Show, FromJSON, ToJSON, ToHttpApiData, IsString)

newtype URL = URL {unwrap :: Text}
    deriving newtype (Eq, Ord, Show, FromJSON, IsString, ToHttpApiData)

newtype URI = URI {unwrap :: Text}
    deriving newtype (Eq, Ord, Show, FromJSON, ToJSON, IsString)

newtype Country = Country {unwrap :: Text}
    deriving newtype (Eq, Ord, Show, FromJSON, ToJSON, ToHttpApiData, IsString)

newtype Locale = Locale {unwrap :: Text}
    deriving newtype (Eq, Ord, Show, FromJSON, ToJSON, ToHttpApiData, IsString)

newtype HTTPError = HTTPError {unwrap :: Int}
    deriving (Show)
    deriving newtype (Eq, Ord, FromJSON)

newtype Restrictions = Restrictions {unwrap :: Map Text Text}
    deriving (Show)
    deriving newtype (Eq, Ord, FromJSON)

newtype ExternalIDs = ExternalIDs {unwrap :: Map Text Text}
    deriving (Show)
    deriving newtype (Eq, Ord, FromJSON)

newtype ExternalURLs = ExternalURLs {unwrap :: Map Text Text}
    deriving (Show)
    deriving newtype (Eq, Ord, FromJSON)

data Scope
    = UgcImageUpload
    | UserModifyPlaybackState
    | UserReadPlaybackState
    | UserReadCurrentlyPlaying
    | UserFollowModify
    | UserFollowRead
    | UserReadRecentlyPlayed
    | UserReadPlaybackPosition
    | UserTopRead
    | PlaylistReadCollaborative
    | PlaylistModifyPublic
    | PlaylistReadPrivate
    | PlaylistModifyPrivate
    | AppRemoteControl
    | Streaming
    | UserReadEmail
    | UserReadPrivate
    | UserLibraryModify
    | UserLibraryRead
    deriving (Eq, Ord, Show, Enum, Bounded)
allScopes :: Set.Set Scope
allScopes = Set.fromList enumerate
showScope :: Scope -> Text
showScope = \case
    UgcImageUpload -> "ugc-image-upload"
    UserModifyPlaybackState -> "user-modify-playback-state"
    UserReadPlaybackState -> "user-read-playback-state"
    UserReadCurrentlyPlaying -> "user-read-currently-playing"
    UserFollowModify -> "user-follow-modify"
    UserFollowRead -> "user-follow-read"
    UserReadRecentlyPlayed -> "user-read-recently-played"
    UserReadPlaybackPosition -> "user-read-playback-position"
    UserTopRead -> "user-top-read"
    PlaylistReadCollaborative -> "playlist-read-collaborative"
    PlaylistModifyPublic -> "playlist-modify-public"
    PlaylistReadPrivate -> "playlist-read-private"
    PlaylistModifyPrivate -> "playlist-modify-private"
    AppRemoteControl -> "app-remote-control"
    Streaming -> "streaming"
    UserReadEmail -> "user-read-email"
    UserReadPrivate -> "user-read-private"
    UserLibraryModify -> "user-library-modify"
    UserLibraryRead -> "user-library-read"

data IDs a = IDs
    { ids :: [a]
    }
    deriving (Eq, Ord, Show, Generic)
    deriving (ToJSON)

newtype ScopeSet = ScopeSet {unwrap :: Set Scope}
instance ToHttpApiData ScopeSet where
    toUrlPiece = toUrlPiece . T.intercalate " " . map showScope . Set.toList . (.unwrap)
