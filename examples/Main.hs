module Main (main) where

import Spotify
import Spotify.CheckPlaylistOverlap qualified
import Spotify.CreateArtistLikedSongsPlaylist qualified
import Spotify.CreatePlaylist qualified
import Spotify.DeleteRecentPlaylists qualified
import Spotify.TransferPlayback qualified
import Spotify.Types.Playlists

import Data.String (fromString)
import Data.Text qualified as T
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
            "CreatePlaylist" : args -> case args of
                [readMaybe . (<> "Search") -> Just searchType, T.pack -> name, readMaybe -> Just public, readMaybe -> Just collaborative, T.pack -> description] ->
                    pure $
                        Spotify.CreatePlaylist.main
                            searchType
                            CreatePlaylistOpts
                                { name
                                , public
                                , collaborative
                                , description
                                }
                _ -> badArgs
            "DeleteRecentPlaylists" : args ->
                case args of
                    [readMaybe -> Just n] -> pure $ Spotify.DeleteRecentPlaylists.main n
                    _ -> badArgs
            "TransferPlayback" : args ->
                case args of
                    [T.pack -> t, readMaybe -> Just b] -> pure $ Spotify.TransferPlayback.main t b
                    _ -> badArgs
            x : _ -> putStrLn ("unknown example: " <> x) >> exitFailure
            [] -> putStrLn "no args" >> exitFailure
    auth <- getAuth -- we use the `MonadSpotify IO` instance, as an easy way to get credentials
    either pPrintForceColor pure =<< runSpotify auth example
  where
    badArgs = putStrLn "bad args" >> exitFailure
