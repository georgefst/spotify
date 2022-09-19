module Spotify.Types.Player where

import Spotify.Types.Internal.CustomJSON
import Spotify.Types.Misc
import Spotify.Types.Tracks

import Data.Aeson (FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data PlaybackState = PlaybackState
    { device :: Device
    , repeatState :: Repeat
    , shuffleState :: Bool
    , context :: Context
    , timestamp :: Int
    , progressMs :: Int
    , isPlaying :: Bool
    , item :: Track
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
    { context :: Context
    , timestamp :: Int
    , progressMs :: Int
    , isPlaying :: Bool
    , item :: Track
    , currentlyPlayingType :: Text
    , actions :: Actions
    }
    deriving (Eq, Ord, Show, Generic)
    deriving (FromJSON) via CustomJSON CurrentlyPlayingTrack

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
    , externalUrls :: ExternalURL
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
