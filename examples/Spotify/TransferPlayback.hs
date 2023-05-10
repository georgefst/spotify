module Spotify.TransferPlayback (main) where

import Spotify
import Spotify.Types.Player

import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_)
import Data.List (find)
import Data.Text (Text)
import Data.Text.IO qualified as T
import System.Exit (exitFailure)

main :: (MonadSpotify m) => Text -> Bool -> m ()
main name play = do
    devs <- getAvailableDevices
    case find ((== name) . (.name)) devs of
        Nothing -> liftIO do
            putStrLn "No device found with requested name. Only:"
            for_ devs \d -> T.putStrLn d.name
            exitFailure
        Just d -> transferPlayback [d.id] play
