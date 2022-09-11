{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Test (tests) where

import Spotify
import Spotify.Servant.Playlists
import Spotify.Types.Misc
import Spotify.Types.Simple
import Spotify.Types.Users

import Control.Monad
import Data.Functor
import Distribution.TestSuite

tests :: IO [Test]
tests = do
    auth <- getAuth
    pure . pure . Test $
        TestInstance
            { run =
                runSpotify auth runAll
                    <&> Finished . \case
                        Left s -> Fail $ show s
                        Right _ -> Pass
            , name = "Spotify tests"
            , tags = []
            , options = []
            , setOption = \_ _ -> Left "no options"
            }

runAll :: Spotify ()
runAll = do
    User{id = me} <- getMe
    getUser me

    getAlbum album1
    getAlbumTracks album1 noPagingParams

    getSavedTracks noPagingParams
    removeAlbums [album2, album3]
    removeTracks [track1, track2]
    saveTracks [track1, track2]

    Paging{items} <- getMyPlaylists $ PagingParams{limit = Just 50, offset = Nothing}
    forM_ (filter ((== playlistName) . (.name)) items) $ unfollowPlaylist . (.id)
    PlaylistSimple{id = playlistId} <-
        createPlaylist
            me
            CreatePlaylistOpts
                { name = playlistName
                , public = False
                , collaborative = False
                , description = "a description"
                }
    addToPlaylist playlistId Nothing $
        map toURI [track3 :: TrackID, track4, track5]
            <> map toURI [podEpisode1 :: EpisodeID]

    getTrack track1

    pure ()
  where
    album1 = "6l8tlcL7JOuG03fAAmcohv" -- push barman
    album2 = "4sIViEgySoHMaqsDLXN45p" -- aerosmith 1
    album3 = "5NHCg9rnAwBPDKOu2LSSUq" -- aerosmith 2
    track1 = "4rTurOCj7TorrnvmacjAFc" -- aerosmith 1.1
    track2 = "6z2uJuztb6zgLpUJJHupv7" -- aerosmith 1.2
    track3 = "7h8bnu16P1gLNCwtiQ1Nk9" -- OTN/MGF
    track4 = "1Qr6phLmzW5Z4k3ggATKh5" -- DSN
    track5 = "2twka2Q4vhCaBMJgtcqLR1" -- CHC
    podEpisode1 = "2T8XOBBkTrqtw8ikXFB2eM" -- Parched
    playlistName = "Haskell test playlist"
