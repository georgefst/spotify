{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Test (tests) where

import Control.Exception
import Control.Monad
import Data.Char
import Data.Composition
import Distribution.TestSuite
import Lens.Micro.Extras
import Text.Pretty.Simple

import qualified Data.Text.IO as T
import Data.Functor ((<&>))

import Types
import Servant

--TODO clean up (e.g. delete playlist)
--TODO run concurrently where possible
--TODO actually check for changes (e.g. query playlist after adding)
--TODO check expected results?
--TODO check that objects don't contain any fields that we ignore (except 'type')
    -- we could also check that 'type' is what we expect
        -- we could even just include 'type' in the objects - it doesn't do any harm
--TODO check results directly in IO as well
--TODO split each chunk (albums, playlists...) in to a separate test case

tests :: IO [Test]
tests = do
    auth <- getAuth
    return $ return $ Test $ TestInstance
        { run = runSpotify Nothing Nothing auth runAll <&> Finished . \case
            Left s -> Fail $ show s
            Right _ -> Pass
        , name = "Spotify tests"
        , tags = []
        , options = []
        , setOption = \_ _ -> Left "no options"
    }

runAll :: Spotify ()
runAll = do
    UserPublic{id = me} <- getMe
    getUser me

    getAlbum album1
    getAlbumTracks album1

    getSavedTracks
    removeAlbums [album2, album3]
    removeTracks [track1, track2]
    saveTracks [track1, track2]

    --TODO doesn't necessarily clean up fully if user has more than 50 playlists -
        -- to be revisited when we handle 'Paging' better
    Paging{items} <- getMyPlaylists (Just 50) Nothing
    forM_ (filter ((== playlistName) . view #name) items) $ unfollowPlaylist . view #id
    PlaylistSimplified{id = playlistId} <- createPlaylist playlistName False False "a description" me
    addTrackToPlaylist [track3,track4,track5] Nothing playlistId
    addEpisodeToPlaylist [podEpisode1] (Just 1) playlistId

    getTrack track1

    return ()
  where
    album1 = "6l8tlcL7JOuG03fAAmcohv" -- push barman
    album2 = "4sIViEgySoHMaqsDLXN45p" -- aerosmith 1 --TODO change these to something good once 'saveTracks' works
    album3 = "5NHCg9rnAwBPDKOu2LSSUq" -- aerosmith 2
    track1 = "4rTurOCj7TorrnvmacjAFc" -- aerosmith 1.1
    track2 = "6z2uJuztb6zgLpUJJHupv7" -- aerosmith 1.2
    track3 = "7h8bnu16P1gLNCwtiQ1Nk9" -- OTN/MGF
    track4 = "1Qr6phLmzW5Z4k3ggATKh5" -- DSN
    track5 = "2twka2Q4vhCaBMJgtcqLR1" -- CHC
    podEpisode1 = "2T8XOBBkTrqtw8ikXFB2eM" -- Parched
    playlistName = "Haskell test playlist"
