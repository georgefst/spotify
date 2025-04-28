module Spotify.Types.Episodes where

import Spotify.Types.Internal.CustomJSON
import Spotify.Types.Misc
import Spotify.Types.Simple

import Data.Aeson (FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data Episode = Episode
    { audioPreviewUrl :: Maybe URL
    , description :: Text
    , htmlDescription :: Text
    , durationMs :: Int
    , explicit :: Bool
    , externalUrls :: ExternalURLs
    , href :: Href
    , id :: EpisodeID
    , images :: [Image]
    , isExternallyHosted :: Bool
    , isPlayable :: Maybe Bool
    , languages :: [Text]
    , name :: Text
    , releaseDate :: Text
    , releaseDatePrecision :: DatePrecision
    , resumePoint :: Maybe ResumePoint
    , uri :: URI
    , restrictions :: Maybe Restrictions
    , show :: ShowSimple
    }
    deriving (Eq, Ord, Show, Generic)
    deriving (FromJSON) via CustomJSON Episode

data SavedEpisode = SavedEpisode
    { addedAt :: Text
    , episode :: Episode
    }
    deriving (Eq, Ord, Show, Generic)
    deriving (FromJSON) via CustomJSON SavedEpisode
