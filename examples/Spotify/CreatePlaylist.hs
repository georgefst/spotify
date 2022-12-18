{- HLINT ignore "Redundant <$>" -}
module Spotify.CreatePlaylist where

import Spotify
import Spotify.Servant.Playlists
import Spotify.Types.Misc
import Spotify.Types.Search
import Spotify.Types.Simple
import Spotify.Types.Tracks
import Spotify.Types.Users

import Control.Monad (void)
import Control.Monad.State (MonadIO (liftIO), MonadState (put), MonadTrans (lift), execStateT)
import Data.List (find)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Traversable (for)
import System.Exit (exitFailure)

main :: MonadSpotify m => CreatePlaylistOpts -> m ()
main opts = do
    liftIO $ T.putStrLn "Enter lines of artist;track"
    parsedLines <-
        (map (T.splitOn ";") . T.lines <$> liftIO T.getContents) >>= traverse \case
            [a, b] -> pure (a, b)
            _ -> exit "parse failure"
    tracks <- for parsedLines \(artist, track) ->
        maybe (exit "not found") pure
            =<< (execStateT @_ @(Maybe Track))
                ( allPages
                    ( Just \p -> do
                        if p.offset > searchLimit
                            then pure False
                            else case find ((artist `elem`) . map (.name) . (.artists)) p.items of
                                Just t -> put (Just t) >> pure True
                                Nothing -> pure False
                    )
                    ( \pp ->
                        maybe (exit "no tracks") pure . (.tracks)
                            =<< lift (search track [TrackSearch] Nothing pp.limit Nothing pp.offset)
                    )
                )
                Nothing
    playlist <- flip createPlaylist opts . (.id) =<< getMe
    void $ addToPlaylist playlist.id Nothing (map (.uri) tracks)
  where
    exit s = liftIO $ T.putStrLn s >> exitFailure

searchLimit :: Int
searchLimit = 250
