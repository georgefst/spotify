module Spotify.Types.Player where

import Data.Function
import Spotify.Types.Episodes
import Spotify.Types.Internal.CustomJSON
import Spotify.Types.Misc
import Spotify.Types.Tracks

import Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import Data.Int (Int64)
import Data.Text (Text)
import GHC.Generics (Generic)

data PlaybackState = PlaybackState
    { device :: Device
    , repeatState :: Repeat
    , shuffleState :: Bool
    , context :: Maybe Context
    , timestamp :: Int64
    , progressMs :: Maybe Int
    , isPlaying :: Bool
    , item :: Maybe PlaybackItem
    , currentlyPlayingType :: Text
    , actions :: Actions
    }
    deriving (Eq, Ord, Show, Generic)
    deriving (FromJSON) via CustomJSON PlaybackState
data Repeat
    = RepeatOff
    | RepeatContext
    | RepeatTrack
    deriving (Eq, Ord, Show, Generic)
    deriving (FromJSON) via CustomJSON Repeat

data CurrentlyPlayingTrack = CurrentlyPlayingTrack
    { context :: Maybe Context
    , timestamp :: Int64
    , progressMs :: Maybe Int
    , isPlaying :: Bool
    , item :: Maybe Track
    , currentlyPlayingType :: Text
    , actions :: Actions
    }
    deriving (Eq, Ord, Show, Generic)
    deriving (FromJSON) via CustomJSON CurrentlyPlayingTrack

data PlaybackItem
    = PlaybackItemTrack Track
    | PlaybackItemEpisode Episode
    deriving (Eq, Ord, Show, Generic)
instance FromJSON PlaybackItem where
    parseJSON v =
        v & withObject "PlaybackItem" \obj -> do
            itemType <- obj .: "type"
            case itemType of
                "track" -> PlaybackItemTrack <$> parseJSON v
                "episode" -> PlaybackItemEpisode <$> parseJSON v
                _ -> fail $ "Unknown item type: " ++ itemType

data Device = Device
    { id :: DeviceID
    , isActive :: Bool
    , isPrivateSession :: Bool
    , isRestricted :: Bool
    , name :: Text
    , type_ :: Text
    , volumePercent :: Int
    }
    deriving (Eq, Ord, Show, Generic)
    deriving (FromJSON) via CustomJSON Device

data Context = Context
    { type_ :: Text
    , href :: Href
    , externalUrls :: ExternalURLs
    , uri :: URI
    }
    deriving (Eq, Ord, Show, Generic)
    deriving (FromJSON) via CustomJSON Context

data Actions = Actions
    { interruptingPlayback :: Maybe Bool
    , pausing :: Maybe Bool
    , resuming :: Maybe Bool
    , seeking :: Maybe Bool
    , skippingNext :: Maybe Bool
    , skippingPrev :: Maybe Bool
    , togglingRepeatContext :: Maybe Bool
    , togglingShuffle :: Maybe Bool
    , togglingRepeatTrack :: Maybe Bool
    , transferringPlayback :: Maybe Bool
    }
    deriving (Eq, Ord, Show, Generic)
    deriving (FromJSON) via CustomJSON Actions
