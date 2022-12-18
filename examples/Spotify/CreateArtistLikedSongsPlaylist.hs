module Spotify.CreateArtistLikedSongsPlaylist (main) where

import Spotify
import Spotify.Servant.Playlists
import Spotify.Types.Artists
import Spotify.Types.Misc
import Spotify.Types.Simple
import Spotify.Types.Tracks
import Spotify.Types.Users

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_, toList)
import Data.List.NonEmpty (NonEmpty)
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Text.IO qualified as T

main :: MonadSpotify m => [ArtistID] -> m ()
main artists = do
    allSaved <- allPages (Just logger) getSavedTracks
    let savedTracksByArtist =
            Map.fromListWith (<>) $
                allSaved >>= \t ->
                    map (\a -> (a.id, pure @NonEmpty t)) t.track.artists
    putT $ "Found tracks by " <> showT (length savedTracksByArtist) <> " artists."
    me <- getMe
    for_ artists \a' -> do
        a <- getArtist a'
        putT $ "Creating playlist for artist: " <> a.name
        p <-
            createPlaylist
                me.id
                CreatePlaylistOpts
                    { name = a.name
                    , public = False
                    , collaborative = False
                    , description = "All my liked songs by this artist. Auto-generated with Haskell."
                    }
        void
            . addToPlaylist p.id Nothing
            . map (\t -> toURI t.track.id)
            . maybe [] toList
            $ Map.lookup a' savedTracksByArtist
  where
    showT = T.pack . show
    putT = liftIO . T.putStrLn
    logger p = do
        putT $
            mconcat
                [ "Getting saved tracks "
                , showT (p.offset + 1)
                , " to "
                , showT (min p.total (p.offset + p.limit))
                , " of "
                , showT p.total
                , "..."
                ]
        pure True
