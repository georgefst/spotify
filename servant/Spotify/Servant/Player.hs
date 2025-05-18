module Spotify.Servant.Player where

import Spotify.Servant.Core
import Spotify.Types.Misc
import Spotify.Types.Player

import Data.Text (Text)
import Servant.API (
    Get,
    JSON,
    PostNoContent,
    PutNoContent,
    QueryParam,
    QueryParam',
    ReqBody,
    Required,
    Strict,
    (:<|>),
    type (:>),
 )

type Player =
    GetPlaybackState
        :<|> TransferPlayback
        :<|> GetAvailableDevices
        :<|> GetCurrentlyPlayingTrack
        :<|> StartPlayback
        :<|> PausePlayback
        :<|> SkipToNext
        :<|> SkipToPrevious
        :<|> SeekToPosition

type GetPlaybackState =
    "me"
        :> "player"
        :> QueryParam "market" Market
        :> QueryParam "additional_types" Text
        :> SpotGetOrNoContent PlaybackState

type TransferPlayback =
    "me"
        :> "player"
        :> ReqBody '[JSON] TransferPlaybackBody
        :> PutNoContent

type GetAvailableDevices =
    "me"
        :> "player"
        :> "devices"
        :> Get '[JSON] GetAvailableDevicesResponse

type GetCurrentlyPlayingTrack =
    "me"
        :> "player"
        :> "currently-playing"
        :> QueryParam "market" Market
        :> Get '[JSON] CurrentlyPlayingTrack

type StartPlayback =
    "me"
        :> "player"
        :> "play"
        :> QueryParam "device_id" DeviceID
        :> ReqBody '[JSON] StartPlaybackOpts
        :> PutNoContent

type PausePlayback =
    "me"
        :> "player"
        :> "pause"
        :> QueryParam "device_id" DeviceID
        :> PutNoContent

type SkipToNext =
    "me"
        :> "player"
        :> "next"
        :> QueryParam "device_id" DeviceID
        :> PostNoContent

type SkipToPrevious =
    "me"
        :> "player"
        :> "previous"
        :> QueryParam "device_id" DeviceID
        :> PostNoContent

type SeekToPosition =
    "me"
        :> "player"
        :> "seek"
        :> QueryParam' '[Strict, Required] "position_ms" Int
        :> QueryParam "device_id" DeviceID
        :> PutNoContent
