module Spotify.DeleteRecentPlaylists (main) where

import Spotify
import Spotify.Types.Misc
import Spotify.Types.Simple

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (for_)
import Data.Text (Text)
import Data.Text.IO qualified as T
import System.IO (hFlush, stdout)

main :: (MonadSpotify m) => Int -> m ()
main n = do
    playlists <- allPages earlyCutoff getMyPlaylists
    for_ (take n playlists) \p -> promptForConfirmation p.name $ unfollowPlaylist p.id
  where
    -- NB. this is just used as an optimisation, to ensure we don't get pages beyond what we need
    earlyCutoff = Just \p -> pure $ p.offset + p.limit < n

promptForConfirmation :: (MonadIO m) => Text -> m () -> m ()
promptForConfirmation t x = do
    liftIO $ T.putStr (t <> "? [Y/n] ") >> hFlush stdout
    liftIO T.getLine >>= \case
        "y" -> x
        "Y" -> x
        "" -> x
        _ -> pure ()
