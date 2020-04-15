-- | [All spotify objects](https://developer.spotify.com/documentation/web-api/reference/object-model/)

{-# LANGUAGE UndecidableInstances #-}

module Types where --TODO rename to Objects?

import Data.Aeson hiding (Error)
import Data.Aeson.Types hiding (Error)
import Data.Char
import Data.Functor.Identity
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Map (Map)

import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State.Lazy

import Data.Function
import Data.Kind
import Data.Time.Calendar
import GHC.Generics
import Type.Reflection

--TODO provide Album typeclass allowing more generic code (also for Artist etc...) ?
data Album = Album
    { albumType :: AlbumType
    , artists    :: [ArtistSimplified]
    , availableMarkets :: [Text]
    , copyrights :: [Copyright]
    , externalIds :: ExternalID
    , externalUrls :: ExternalURL
    , genres :: [Text]
    , href :: Href
    , id :: ID
    , images :: [Image]
    , label :: Text
    , name :: Text
    , popularity :: Int
    , releaseDate :: Text
    , releaseDatePrecision :: DatePrecision --TODO find test example where this is not "day" - check previous field parses - spotify:track:1idiifjTcCoGfBeZiYOdim
    , restrictions :: Maybe Restrictions
    , tracks :: Paging TrackSimplified
    , uri :: URI
}   deriving (Eq, Ord, Show, Generic)
    deriving FromJSON via Record Album

data AlbumSimplified = AlbumSimplified
    { albumType :: AlbumType
    , artists    :: [ArtistSimplified]
    , availableMarkets :: [Text]
    , externalUrls :: ExternalURL
    , albumGroup :: Maybe AlbumGroup
    , href :: Href
    , id :: ID
    , images :: [Image]
    , name :: Text
    , releaseDate :: Text --TODO work out general format - seems to vary a lot
    , releaseDatePrecision :: DatePrecision
    , restrictions :: Maybe Restrictions
    , uri :: URI
}   deriving (Eq, Ord, Show, Generic)
    deriving FromJSON via Record AlbumSimplified

data Artist = Artist
    { externalUrls :: ExternalURL
    , followers :: Followers
    , genres :: [Genre]
    , href :: Href
    , id :: ID
    , images :: [Image]
    , name :: Text
    , popularity :: Int
    , uri :: URI
}   deriving (Eq, Ord, Show, Generic)
    deriving FromJSON via Record Artist

data ArtistSimplified = ArtistSimplified
    { externalUrls :: ExternalURL
    , href :: Href
    , id :: ID
    , name :: Text
    , uri :: URI
}   deriving (Eq, Ord, Show, Generic)
    deriving FromJSON via Record ArtistSimplified

data AudioFeatures = AudioFeatures
    { acousticness :: Float
    , analysisUrl :: URL
    , danceability :: Float
    , durationMs :: Int
    , energy :: Float
    , id :: ID
    , instrumentalness :: Float
    , key :: Key --TODO run tests that int is always between 0 and 11
    , liveness :: Float
    , loudness :: Float
    , mode :: Modality --TODO run tests that int is always 0 or 1
    , speechiness :: Float
    , tempo :: Float
    , timeSignature :: Int
    , trackHref :: Href
    , uri :: URI
    , valence :: Float
}   deriving (Eq, Ord, Show, Generic)
    deriving FromJSON via Record AudioFeatures

data Category = Category
    { href :: Href
    , icons :: [Image]
    , id :: ID
    , name :: Text
}   deriving (Eq, Ord, Show, Generic)
    deriving FromJSON via Record Category

data Context = Context
    { href :: Href
    , externalUrls :: ExternalURL
    , uri :: URI
}   deriving (Eq, Ord, Show, Generic)
    deriving FromJSON via Record Context

--TODO not currently listed in (old) documentation on object model
data CurrentlyPlaying = CurrentlyPlaying
    { actions :: Actions
    , context :: Context
    , currentlyPlayingType :: Text
    , device :: Device
    , isPlaying :: Bool
    , item :: Track
    , progressMs :: Int
    , repeatState :: Text
    , shuffleState :: Bool
    , timestamp :: Int
}   deriving (Eq, Ord, Show, Generic)
    deriving FromJSON via Record CurrentlyPlaying

data Copyright = Copyright
    { text :: Text
    --TODO we're gonna need a partially manual instance here - there's a genuinely useful field called 'type'
    -- , copyrightType :: CopyrightType
}   deriving (Eq, Ord, Show, Generic)
    deriving FromJSON via Record Copyright

data Cursor = Cursor
    { after :: Text
}   deriving (Eq, Ord, Show, Generic)
    deriving FromJSON via Record Cursor

data Disallows = Disallows
    { interruptingPlayback :: Bool
    , pausing :: Bool
    , resuming :: Bool
    , seeking :: Bool
    , skippingNext :: Bool
    , skippingPrev :: Bool
    , togglingRepeatContext :: Bool
    , togglingShuffle :: Bool
    , togglingRepeatTrack :: Bool
    , transferringPlayback :: Bool
}   deriving (Eq, Ord, Show, Generic)
    deriving FromJSON via Record Disallows

data Error = Error
    { status :: HTTPError
    , message :: Text
}   deriving (Eq, Ord, Show, Generic)
    deriving FromJSON via Record Error

data PlayerError = PlayerError
    { status :: HTTPError
    , message :: Text
    , reason :: PlayerErrorReason
}   deriving (Eq, Ord, Show, Generic)
    deriving FromJSON via Record PlayerError

data PlayerErrorReason
    = NoPrevTrack
    | NoNextTrack
    | NoSpecificTrack
    | AlreadyPaused
    | NotPaused
    | NotPlayingLocally
    | NotPlayingTrack
    | NotPlayingContext
    | EndlessContext
    | ContextDisallow
    | AlreadyPlaying
    | RateLimited
    | RemoteControlDisallow
    | DeviceNotControllable
    | VolumeControlDisallow
    | NoActiveDevice
    | PremiumRequired
    | Unknown
    deriving (Eq, Ord, Show, Generic)
    deriving FromJSON via Record PlayerErrorReason

data Followers = Followers
    { href :: Maybe Text
    , total :: Int
}   deriving (Eq, Ord, Show, Generic)
    deriving FromJSON via Record Followers

data Image = Image
    { height :: Maybe Int
    , url :: Text
    , width :: Maybe Int
}   deriving (Eq, Ord, Show, Generic)
    deriving FromJSON via Record Image

data Paging a = Paging
    { href :: Text
    , items :: [a]
    , limit :: Int
    , next :: Maybe Text
    , offset :: Int
    , previous :: Maybe Text
    , total :: Int
}   deriving (Eq, Ord, Show, Generic)
    deriving FromJSON via Record (Paging a)

data Track = Track
    { album :: AlbumSimplified
    , artists :: [ArtistSimplified]
    , availableMarkets :: Maybe [Text]
    , discNumber :: Int
    , durationMs :: Int
    , explicit :: Bool
    , externalIds :: ExternalID
    , externalUrls :: ExternalURL
    , href :: Text
    , id :: ID
    , isPlayable :: Maybe Bool
    , linkedFrom :: Maybe TrackLink
    , restrictions :: Maybe Restrictions
    , name :: Text
    , popularity :: Int
    , previewUrl :: Maybe Text
    , trackNumber :: Int
    , uri :: URI
}   deriving (Eq, Ord, Show, Generic)
    deriving FromJSON via Record Track

data TrackSimplified = TrackSimplified
    { artists :: [ArtistSimplified]
    , availableMarkets :: Maybe [Text]
    , discNumber :: Int
    , durationMs :: Int -- Change to some time type?
    , explicit :: Bool
    , externalUrls :: ExternalURL
    , href :: Text
    , id :: ID
    , isPlayable :: Maybe Bool -- only present when relinking applied
    , linkedFrom :: Maybe TrackLink -- only present when relinking applied
    , name :: Text
    , previewUrl :: Maybe Text
    , trackNumber :: Int
    , uri :: URI
}   deriving (Eq, Ord, Show, Generic)
    deriving FromJSON via Record TrackSimplified

data TrackLink = TrackLink
    { externalUrls :: ExternalURL
    , href :: Text
    , id :: ID
    , url :: Text
}   deriving (Eq, Ord, Show, Generic)
    deriving FromJSON via Record TrackLink

type Restrictions = Map Text Text -- Documentation is unclear how this object is formed

data PlaylistSimplified = PlaylistSimplified
    { collaborative :: Bool
    , externalUrls :: ExternalURL
    , href :: Text
    , id :: ID
    , images :: [Image]
    , name :: Text
    , owner :: UserPublic
    , public :: Maybe Bool
    , snapshotId :: ID
    , tracks :: Tracks
    , uri :: URI
}   deriving (Eq, Ord, Show, Generic)
    deriving FromJSON via Record PlaylistSimplified

data Tracks = Tracks
    { href :: Maybe Text
    , total :: Int
}   deriving (Eq, Ord, Show, Generic)
    deriving FromJSON via Record Tracks

data UserPublic = UserPublic
    { displayName :: Maybe Text
    , externalUrls :: ExternalURL
    , followers :: Maybe Followers -- the Spotify user does not have this field
    , href :: Text
    , id :: ID
    , images :: Maybe [Image] -- the Spotify user does not have this field (this should be reported to the API developers)
    , uri :: URI
}   deriving (Eq, Ord, Show, Generic)
    deriving FromJSON via Record UserPublic

data SearchType = SearchTypeAlbum | SearchTypeArtist | SearchTypeTrack deriving (Show, Enum, Bounded)

searchTypeString :: SearchType -> String
searchTypeString SearchTypeAlbum = "album"
searchTypeString SearchTypeArtist = "artist"
searchTypeString SearchTypeTrack = "track"

--TODO
type Genre = Text
type Href = Text
type ID = Text
type URL = Text
type URI = Text
type HTTPError = Int
type ExternalID = Map Text Text
type ExternalURL = Map Text Text
--TODO cursor-based paging, play history and onward

--TODO type some of these fields
data TokenResponse = TokenResponse
    { accessToken :: Text
    , tokenType :: Text
    , expiresIn :: Int
    , scope :: Text
}   deriving (Eq, Ord, Show, Generic)
    deriving FromJSON via Record TokenResponse

--TODO
-- The API spec says that the message should always be present. In practice, it seems to never be present.
-- This field could be removed if we can confirm that no message is given in response.
-- It might be worth reporting this as an issue to the developers of the API.
data NewReleasesResponse = NewReleasesResponse
    { message :: Maybe Text
    , message :: Paging AlbumSimplified
}   deriving (Eq, Ord, Show, Generic)
    deriving FromJSON via Record NewReleasesResponse

data FeaturedPlaylistsResponse = FeaturedPlaylistsResponse
    { message :: Maybe Text
    , playlists :: Paging PlaylistSimplified
}   deriving (Eq, Ord, Show, Generic)
    deriving FromJSON via Record FeaturedPlaylistsResponse

data CategoriesResponse = CategoriesResponse
    { categories :: Paging Category
}   deriving (Eq, Ord, Show, Generic)
    deriving FromJSON via Record CategoriesResponse

data CategoryPlaylistsResponse = CategoryPlaylistsResponse
    { playlists :: Paging PlaylistSimplified
}   deriving (Eq, Ord, Show, Generic)
    deriving FromJSON via Record CategoryPlaylistsResponse

data SearchResponse = SearchResponse
    { artists :: Maybe (Paging Artist)
    , albums :: Maybe (Paging AlbumSimplified)
    , tracks :: Maybe (Paging Track)
}   deriving (Eq, Ord, Show, Generic)
    deriving FromJSON via Record SearchResponse

--TODO just use 'type' ?
data Devices = Devices [Device]
    deriving (Eq, Ord, Show, Generic)
    deriving FromJSON via Record Devices
data Device = Device
    { id :: Text
    , isActive :: Bool
    , isPrivateSession :: Bool
    , isRestricted :: Bool
    , name :: Text
    , volumePercent :: Int
}   deriving (Eq, Ord, Show, Generic)
    deriving FromJSON via Record Device

data Actions = Actions {
    disallows :: Disallows
}   deriving (Eq, Ord, Show, Generic)
    deriving FromJSON via Record Actions


{- Util -}

newtype Record a = Record a deriving Generic
instance (Generic a, GFromJSON Zero (Rep a)) => FromJSON (Record a) where
    parseJSON = fmap Record . genericParseJSON defaultOptions{constructorTagModifier,fieldLabelModifier}
        where
            fieldLabelModifier = camelToSnake
            constructorTagModifier = camelToSnake
newtype Sum a = Sum a deriving Generic
instance (Generic a, GFromJSON Zero (Rep a), Typeable a) => FromJSON (Sum a) where
    parseJSON = fmap Sum . genericParseJSON defaultOptions{constructorTagModifier}
        where constructorTagModifier = camelToSnake . drop (length $ show (typeRep @a))

camelToSnake :: String -> String
camelToSnake = \case
    [] -> []
    x:xs -> toLower x : go xs
    where go = \case
            []   -> []
            x:xs ->
                if isUpper x then
                    '_' : toLower x : go xs
                else
                    x : go xs

data AccessToken = AccessToken
    { tokenText :: Text
    , expiresIn :: POSIXTime
}   deriving (Eq, Ord, Show, Generic)
    deriving FromJSON via Sum AccessToken


{- Other types -}

data AlbumGroup
    = GroupAlbum
    | GroupSingle
    | GroupCompilation
    | AppearsOn
    deriving (Eq, Ord, Show, Generic)
    deriving FromJSON via Sum AlbumGroup

data AlbumType
    = AlbumTypeAlbum
    | AlbumTypeSingle
    | AlbumTypeCompilation
    deriving (Eq, Ord, Show, Generic)
    deriving FromJSON via Sum AlbumType

data DatePrecision
    = DatePrecisionYear
    | DatePrecisionMonth
    | DatePrecisionDay
    deriving (Eq, Ord, Show, Generic)
    deriving FromJSON via Sum DatePrecision

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
instance FromJSON Key where
    parseJSON = fmap toEnum . parseJSON

--TODO what about other modalities? I guess Spotify doesn't recognise them
data Modality
    = Minor
    | Major
    deriving (Eq, Ord, Show, Generic, Enum)
instance FromJSON Modality where
    parseJSON = fmap toEnum . parseJSON

data CopyrightType
    = C
    | P
    deriving (Eq, Ord, Show, Generic)
    deriving FromJSON via Sum CopyrightType
