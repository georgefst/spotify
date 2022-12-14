module Main (main) where

import Spotify
import Spotify.CheckPlaylistOverlap qualified
import Spotify.CreateArtistLikedSongsPlaylist qualified
import Spotify.DeleteRecentPlaylists qualified

import Data.String (fromString)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.Pretty.Simple (pPrintForceColor)
import Text.Read (readMaybe)

main :: IO ()
main = do
    example <-
        getArgs >>= \case
            "CheckPlaylistOverlap" : args -> case args of
                [fromString -> p1, fromString -> p2] -> pure $ Spotify.CheckPlaylistOverlap.main p1 p2
                _ -> badArgs
            "CreateArtistLikedSongsPlaylist" : artists ->
                pure $ Spotify.CreateArtistLikedSongsPlaylist.main $ map fromString artists
            "DeleteRecentPlaylists" : args ->
                case args of
                    [readMaybe -> Just time] -> pure $ Spotify.DeleteRecentPlaylists.main time
                    _ -> badArgs
            x : _ -> putStrLn ("unknown example: " <> x) >> exitFailure
            [] -> putStrLn "no args" >> exitFailure
    auth <- getAuth -- we use the `MonadSpotify IO` instance, as an easy way to get credentials
    either pPrintForceColor pure =<< runSpotify auth example
  where
    badArgs = putStrLn "bad args" >> exitFailure
