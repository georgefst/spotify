module Spotify.Types.Search where

import Spotify.Types.Artists
import Spotify.Types.Internal.CustomJSON
import Spotify.Types.Misc
import Spotify.Types.Simple
import Spotify.Types.Tracks

import Data.Aeson (FromJSON, Value)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Servant.API (ToHttpApiData (toUrlPiece))

data SearchType
    = AlbumSearch
    | ArtistSearch
    | PlaylistSearch
    | TrackSearch
    | ShowSearch
    | EpisodeSearch
    | AudiobookSearch
    deriving (Eq, Ord, Show, Read, Generic)
instance ToHttpApiData [SearchType] where
    toUrlPiece =
        T.intercalate "," . map \case
            AlbumSearch -> "album"
            ArtistSearch -> "artist"
            PlaylistSearch -> "playlist"
            TrackSearch -> "track"
            ShowSearch -> "show"
            EpisodeSearch -> "episode"
            AudiobookSearch -> "audiobook"

data SearchResult = SearchResult
    { tracks :: Maybe (Paging Track)
    , artists :: Maybe (Paging Artist)
    , albums :: Maybe (Paging AlbumSimple)
    , playlists :: Maybe (Paging PlaylistSimple)
    , shows :: Maybe (Paging Value)
    , episodes :: Maybe (Paging Value)
    , audiobooks :: Maybe (Paging Value)
    }
    deriving (Eq, Ord, Show, Generic)
    deriving (FromJSON) via CustomJSON SearchResult
