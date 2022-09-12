module Main (main) where

import Spotify
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
            "CreateArtistLikedSongsPlaylist" : artists ->
                pure $ Spotify.CreateArtistLikedSongsPlaylist.main $ map fromString artists
            "DeleteRecentPlaylists" : args ->
                case args of
                    [readMaybe -> Just time] -> pure $ Spotify.DeleteRecentPlaylists.main time
                    _ -> putStrLn "bad args" >> exitFailure
            x : _ -> putStrLn ("unknown example: " <> x) >> exitFailure
            [] -> putStrLn "no args" >> exitFailure
    auth <- getAuth -- we use the `MonadSpotify IO` instance, as an easy way to get credentials
    either pPrintForceColor pure =<< runSpotify auth example
