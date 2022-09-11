module Spotify.Types.Tracks where

import Spotify.Types.Internal.CustomJSON
import Spotify.Types.Internal.EnumJSON
import Spotify.Types.Misc
import Spotify.Types.Simple qualified as Simple

import Data.Aeson (FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data Track = Track
    { album :: Simple.Album
    , artists :: [Simple.Artist]
    , availableMarkets :: Maybe [Text]
    , discNumber :: Int
    , durationMs :: Int
    , explicit :: Bool
    , externalIds :: ExternalID
    , externalUrls :: ExternalURL
    , href :: Href
    , id :: ID
    , isPlayable :: Maybe Bool
    , linkedFrom :: Maybe TrackLink
    , restrictions :: Maybe Restrictions
    , name :: Text
    , popularity :: Int
    , previewUrl :: Maybe Text
    , trackNumber :: Int
    , uri :: URI
    }
    deriving (Eq, Ord, Show, Generic)
    deriving (FromJSON) via CustomJSON Track

data AudioFeatures = AudioFeatures
    { acousticness :: Float
    , analysisUrl :: URL
    , danceability :: Float
    , durationMs :: Int
    , energy :: Float
    , id :: ID
    , instrumentalness :: Float
    , key :: Key
    , liveness :: Float
    , loudness :: Float
    , mode :: Modality
    , speechiness :: Float
    , tempo :: Float
    , timeSignature :: Int
    , trackHref :: Href
    , uri :: URI
    , valence :: Float
    }
    deriving (Eq, Ord, Show, Generic)
    deriving (FromJSON) via CustomJSON AudioFeatures

data Modality
    = Minor
    | Major
    deriving (Eq, Ord, Show, Generic, Enum)
    deriving (FromJSON) via EnumJSON Modality

data SavedTrack = SavedTrack
    { addedAt :: Text
    , track :: Track
    }
    deriving (Eq, Ord, Show, Generic)
    deriving (FromJSON) via CustomJSON SavedTrack
