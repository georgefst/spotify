{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Test (tests) where

import Spotify
import Spotify.Types.Misc
import Spotify.Types.Simple qualified as Simple
import Spotify.Types.Users

import Control.Monad
import Data.Functor
import Data.Text qualified as T
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
    Simple.Playlist{id = playlistId} <- createPlaylist me playlistName False False $ T.pack "a description"
    addToPlaylist playlistId Nothing $
        map (idToURI $ T.pack "track") [track3, track4, track5]
            <> map (idToURI $ T.pack "episode") [podEpisode1]

    getTrack track1

    pure ()
  where
    album1 = ID $ T.pack "6l8tlcL7JOuG03fAAmcohv" -- push barman
    album2 = ID $ T.pack "4sIViEgySoHMaqsDLXN45p" -- aerosmith 1
    album3 = ID $ T.pack "5NHCg9rnAwBPDKOu2LSSUq" -- aerosmith 2
    track1 = ID $ T.pack "4rTurOCj7TorrnvmacjAFc" -- aerosmith 1.1
    track2 = ID $ T.pack "6z2uJuztb6zgLpUJJHupv7" -- aerosmith 1.2
    track3 = ID $ T.pack "7h8bnu16P1gLNCwtiQ1Nk9" -- OTN/MGF
    track4 = ID $ T.pack "1Qr6phLmzW5Z4k3ggATKh5" -- DSN
    track5 = ID $ T.pack "2twka2Q4vhCaBMJgtcqLR1" -- CHC
    podEpisode1 = ID $ T.pack "2T8XOBBkTrqtw8ikXFB2eM" -- Parched
    playlistName = T.pack "Haskell test playlist"
