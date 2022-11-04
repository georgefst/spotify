module Spotify.Servant.Player where

import Spotify.Servant.Core
import Spotify.Types.Internal.CustomJSON
import Spotify.Types.Misc
import Spotify.Types.Player

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Servant.API (
    JSON,
    QueryParam,
    QueryParam',
    ReqBody,
    Required,
    Strict,
    type (:>),
 )

type GetPlaybackState =
    "me"
        :> "player"
        :> QueryParam "market" Market
        :> SpotGet PlaybackState

type TransferPlayback =
    "me"
        :> "player"
        :> ReqBody '[JSON] TransferPlaybackBody
        :> SpotPutNoContent
data TransferPlaybackBody = TransferPlaybackBody
    { device_ids :: [DeviceID]
    , play :: Bool
    }
    deriving (Eq, Ord, Show, Generic)
    deriving (ToJSON)

type GetAvailableDevices =
    "me"
        :> "player"
        :> "devices"
        :> SpotGet GetAvailableDevicesResponse
newtype GetAvailableDevicesResponse = GetAvailableDevicesResponse
    { devices :: [Device]
    }
    deriving (Eq, Ord, Show, Generic)
    deriving (FromJSON) via CustomJSON GetAvailableDevicesResponse

type GetCurrentlyPlayingTrack =
    "me"
        :> "player"
        :> "currently-playing"
        :> QueryParam "market" Market
        :> SpotGet CurrentlyPlayingTrack

type StartPlayback =
    "me"
        :> "player"
        :> "play"
        :> QueryParam "device_id" DeviceID
        :> ReqBody '[JSON] StartPlaybackOpts
        :> SpotPutNoContent
data StartPlaybackOpts = StartPlaybackOpts
    { context_uri :: Maybe URI
    , uris :: Maybe [URI]
    , offset :: Maybe Offset
    }
    deriving (Eq, Ord, Show, Generic)
    deriving (ToJSON)
emptyStartPlaybackOpts :: StartPlaybackOpts
emptyStartPlaybackOpts = StartPlaybackOpts Nothing Nothing Nothing

type PausePlayback =
    "me"
        :> "player"
        :> "pause"
        :> QueryParam "device_id" DeviceID
        :> SpotPutNoContent

type SkipToNext =
    "me"
        :> "player"
        :> "next"
        :> QueryParam "device_id" DeviceID
        :> SpotPostNoContent

type SkipToPrevious =
    "me"
        :> "player"
        :> "previous"
        :> QueryParam "device_id" DeviceID
        :> SpotPostNoContent

type SeekToPosition =
    "me"
        :> "player"
        :> "seek"
        :> QueryParam' '[Strict, Required] "position_ms" Int
        :> QueryParam "device_id" DeviceID
        :> SpotPutNoContent
