module Main (main) where

import Spotify
import Spotify.CreateArtistLikedSongsPlaylist qualified

import Data.String (fromString)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.Pretty.Simple (pPrintForceColor)

main :: IO ()
main = do
    getArgs >>= \case
        "CreateArtistLikedSongsPlaylist" : artists -> do
            let example = Spotify.CreateArtistLikedSongsPlaylist.main $ map fromString artists
            auth <- getAuth -- we use the `MonadSpotify IO` instance, as an easy way to get credentials
            either pPrintForceColor pure =<< runSpotify auth example
        x : _ -> putStrLn ("unknown example: " <> x) >> exitFailure
        [] -> putStrLn "no args" >> exitFailure
