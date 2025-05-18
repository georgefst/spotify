{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- | Raw bindings directly from the Servant API.
module Spotify.Client where

import Spotify.Servant qualified

import Data.Proxy (Proxy (Proxy))
import Servant.API (type (:<|>) ((:<|>)))
import Servant.API.Flatten (flatten)
import Servant.Client (client)

getAlbum
    :<|> getAlbumTracks
    :<|> removeAlbums
    :<|> getArtist
    :<|> getCategories
    :<|> getEpisode
    :<|> getSavedEpisodes
    :<|> saveEpisodes
    :<|> removeEpisodes
    :<|> getPlaybackState
    :<|> transferPlayback
    :<|> getAvailableDevices
    :<|> getCurrentlyPlayingTrack
    :<|> startPlayback
    :<|> pausePlayback
    :<|> skipToNext
    :<|> skipToPrevious
    :<|> seekToPosition
    :<|> getPlaylist
    :<|> addToPlaylist
    :<|> getMyPlaylists
    :<|> createPlaylist
    :<|> getSearch
    :<|> getTrack
    :<|> getSavedTracks
    :<|> saveTracks
    :<|> removeTracks
    :<|> getMe
    :<|> getUser
    :<|> unfollowPlaylist =
        client (flatten $ Proxy @Spotify.Servant.API)

refreshAccessToken
    :<|> requestAccessToken
    :<|> authorize =
        client $ Proxy @Spotify.Servant.AccountsAPI
