{- HLINT ignore "Redundant <$>" -}
module Spotify.CreatePlaylist (main) where

import Spotify
import Spotify.Servant.Playlists
import Spotify.Types.Albums
import Spotify.Types.Misc
import Spotify.Types.Search
import Spotify.Types.Simple
import Spotify.Types.Tracks
import Spotify.Types.Users

import Control.Monad (void, (<=<))
import Control.Monad.State (MonadIO (liftIO), MonadState (put), MonadTrans (lift), runStateT)
import Data.List (find)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Traversable (for)
import System.Exit (exitFailure)

main :: MonadSpotify m => SearchType -> CreatePlaylistOpts -> m ()
main searchType opts = do
    SomeSearchTypeInfo @a (itemName, extractItems, getResult, getUris) <- case searchType of
        TrackSearch -> pure $ SomeSearchTypeInfo @Track ("track", (.tracks), (.artists), pure . pure . (.uri))
        AlbumSearch -> pure $ SomeSearchTypeInfo @AlbumSimple ("album", (.albums), (.artists), fmap (map (.uri) . (.tracks.items)) . getAlbum . (.id))
        _ -> exit "unsupported search type"
    liftIO $ T.putStrLn $ "Enter lines of artist;" <> itemName
    parsedLines <-
        (map (T.splitOn ";") . T.lines <$> liftIO T.getContents) >>= traverse \case
            [a, b] -> pure (a, b)
            _ -> exit "parse failure"
    items <- for parsedLines \(artist, item) ->
        runStateT @(Maybe a)
            ( allPages
                ( Just \p -> do
                    if p.offset > searchLimit
                        then pure False
                        else case find ((artist `elem`) . map (.name) . getResult) p.items of
                            Just t -> put (Just t) >> pure True
                            Nothing -> pure False
                )
                ( maybe (exit $ "no " <> itemName <> "s") pure . extractItems
                    <=< lift . search (T.unwords [item, artist]) [searchType] Nothing Nothing
                )
            )
            Nothing
            >>= \(searched, res) ->
                maybe
                    ( exit . T.unlines $
                        ("No " <> itemName <> " \"" <> item <> "\" with artist: \"" <> artist <> "\". Found:")
                            : map (T.intercalate "; " . map (.name) . getResult) searched
                    )
                    pure
                    res
    playlist <- flip createPlaylist opts . (.id) =<< getMe
    void $ addToPlaylist playlist.id Nothing . concat =<< traverse getUris items
  where
    exit s = liftIO $ T.putStrLn s >> exitFailure

data SomeSearchTypeInfo where
    SomeSearchTypeInfo ::
        ( Text
        , SearchResult -> Maybe (Paging a)
        , a -> [ArtistSimple]
        , forall m. MonadSpotify m => a -> m [URI]
        ) ->
        SomeSearchTypeInfo

searchLimit :: Int
searchLimit = 250
