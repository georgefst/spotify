{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- | Raw bindings directly from the Servant API.
module Spotify.Client where

import Spotify.Servant qualified

import Data.Proxy (Proxy (Proxy))
import Servant.API (type (:<|>) ((:<|>)))
import Servant.API.Flatten (flatten)
import Servant.Client (client)

getAlbum0
    :<|> getAlbumTracks0
    :<|> removeAlbums0
    :<|> getArtist0
    :<|> getCategories0
    :<|> getEpisode0
    :<|> getSavedEpisodes0
    :<|> saveEpisodes0
    :<|> removeEpisodes0
    :<|> getPlaybackState0
    :<|> transferPlayback0
    :<|> getAvailableDevices0
    :<|> getCurrentlyPlayingTrack0
    :<|> startPlayback0
    :<|> pausePlayback0
    :<|> skipToNext0
    :<|> skipToPrevious0
    :<|> seekToPosition0
    :<|> getPlaylist0
    :<|> addToPlaylist0
    :<|> getMyPlaylists0
    :<|> createPlaylist0
    :<|> getSearch0
    :<|> getTrack0
    :<|> getSavedTracks0
    :<|> saveTracks0
    :<|> removeTracks0
    :<|> getMe0
    :<|> getUser0
    :<|> unfollowPlaylist0 =
        client (flatten $ Proxy @Spotify.Servant.API)

refreshAccessToken0
    :<|> requestAccessToken0
    :<|> authorize0 =
        client $ Proxy @Spotify.Servant.AccountsAPI
