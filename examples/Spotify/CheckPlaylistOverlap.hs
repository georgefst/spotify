module Spotify.CheckPlaylistOverlap (main) where

import Spotify
import Spotify.Types.Misc
import Spotify.Types.Playlists
import Spotify.Types.Tracks

import Control.Monad (join, (<=<))
import Control.Monad.IO.Class (liftIO)
import Data.Bitraversable (Bitraversable (bitraverse))
import Data.Foldable (traverse_)
import Data.List.NonEmpty (nonEmpty)
import Data.Map qualified as Map
import Data.Text.IO qualified as T
import Data.Tuple.Extra ((&&&))

main :: (MonadSpotify m) => PlaylistID -> PlaylistID -> m ()
main =
    curry $
        maybe (putT "No overlap found.") ((putT "Found songs in both playlists:" >>) . traverse_ putT)
            . nonEmpty
            . Map.elems
            . uncurry Map.intersection
            <=< join
                bitraverse
                ( pure . Map.fromList . map (((.id) &&& (.name)) . (.track))
                    <=< (allPages Nothing . const . pure . (.tracks))
                    <=< getPlaylist
                )
  where
    putT = liftIO . T.putStrLn
