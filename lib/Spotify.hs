module Spotify (
    MonadSpotify (..),
    Spotify (..),
    runSpotify,
    Auth (..),
    mainBase,
    accountsBase,
    noContent,
    marketFromToken,
    withPagingParams,
    PagingParams(..),
    noPagingParams,
    newToken,
    newTokenIO,
    newTokenIO',
    getAuthCodeInteractive,
    authorizeUrl,
    getAlbum,
    getAlbumTracks,
    removeAlbums,
    getArtist,
    getTrack,
    getSavedTracks,
    saveTracks,
    removeTracks,
    search,
    getMe,
    getUser,
    unfollowPlaylist,
    getPlaylist,
    addToPlaylist,
    getMyPlaylists,
    createPlaylist,
    getCategories,
    getEpisode,
    getSavedEpisodes,
    saveEpisodes,
    removeEpisodes,
    getPlaybackState,
    transferPlayback,
    getAvailableDevices,
    getCurrentlyPlayingTrack,
    startPlayback,
    pausePlayback,
    skipToNext,
    skipToPrevious,
    seekToPosition,
    allPages,
    module Spotify.Servant.Core,
    module Spotify.Types.Albums,
    module Spotify.Types.Artists,
    module Spotify.Types.Auth,
    module Spotify.Types.Categories,
    module Spotify.Types.Episodes,
    module Spotify.Types.Misc,
    module Spotify.Types.Player,
    module Spotify.Types.Playlists,
    module Spotify.Types.Search,
    module Spotify.Types.Simple,
    module Spotify.Types.Tracks,
    module Spotify.Types.Users,
)
where

import Spotify.Client qualified
import Spotify.Servant qualified
import Spotify.Servant.Core
import Spotify.Types.Albums
import Spotify.Types.Artists
import Spotify.Types.Auth
import Spotify.Types.Categories
import Spotify.Types.Episodes
import Spotify.Types.Misc
import Spotify.Types.Player
import Spotify.Types.Playlists
import Spotify.Types.Search
import Spotify.Types.Simple
import Spotify.Types.Tracks
import Spotify.Types.Users

import Control.Applicative ((<|>))
import Control.Exception (throwIO)
import Control.Monad.Except (ExceptT, MonadError, liftEither, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Loops (unfoldrM)
import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
import Control.Monad.State (MonadState, StateT, evalStateT, get, put)
import Data.Aeson (FromJSON, eitherDecode)
import Data.Bifunctor (bimap)
import Data.Coerce (coerce)
import Data.Composition ((.:), (.:.))
import Data.Proxy (Proxy (Proxy))
import Data.Set (Set)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import GHC.Generics (Generic)
import Network.HTTP.Client (Manager)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types (Status (statusCode))
import Servant.API (NoContent (NoContent), (:<|>) ((:<|>)))
import Servant.Client (BaseUrl (BaseUrl, baseUrlHost), ClientError (DecodeFailure, FailureResponse), ClientM, Scheme (Https), mkClientEnv, responseBody, responseStatusCode, runClientM)
import Servant.Links (allLinks, linkURI)
import System.Directory (XdgDirectory (XdgConfig), createDirectoryIfMissing, getTemporaryDirectory, getXdgDirectory)
import System.FilePath ((</>))
import System.IO (hFlush, stdout)

class (MonadIO m) => MonadSpotify m where
    getAuth :: m Auth
    getManager :: m Manager
    getToken :: m AccessToken
    putToken :: AccessToken -> m ()
    throwClientError :: ClientError -> m a

instance MonadSpotify IO where
    throwClientError = liftIO . throwIO
    getAuth = do
        dir <- getXdgDirectory XdgConfig "spotify-haskell"
        let getData file prompt =
                -- look for file - otherwise get from stdin
                T.readFile path <|> do
                    res <- T.putStr (prompt <> ": ") >> hFlush stdout >> T.getLine
                    createDirectoryIfMissing False dir
                    T.writeFile path res
                    pure res
              where
                path = dir </> file
        Auth
            <$> (RefreshToken <$> getData "refresh" "Refresh token")
            <*> (ClientId <$> getData "id" "Client id")
            <*> (ClientSecret <$> getData "secret" "Client secret")
    getManager = newTlsManager
    getToken = do
        path <- monadSpotifyIOTokenPath
        AccessToken <$> T.readFile path <|> do
            TokenResponse{accessToken} <- newToken
            putToken accessToken
            pure accessToken
    putToken (AccessToken t) = do
        path <- monadSpotifyIOTokenPath
        T.writeFile path t
monadSpotifyIOTokenPath :: IO FilePath
monadSpotifyIOTokenPath = (</> "spotify-haskell-token") <$> getTemporaryDirectory

newtype Spotify a = Spotify
    { unwrap :: StateT AccessToken (ReaderT (Auth, Manager) (ExceptT ClientError IO)) a
    }
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadState AccessToken
        , MonadReader (Auth, Manager)
        , MonadError ClientError
        )

instance MonadSpotify Spotify where
    getAuth = asks fst
    getManager = asks snd
    getToken = get
    putToken = put
    throwClientError = throwError

runSpotify :: Auth -> Spotify a -> IO (Either ClientError a)
runSpotify auth (Spotify x) = runExceptT do
    man <- newTlsManager
    tok <- liftEither =<< liftIO (newTokenIO auth man)
    runReaderT (evalStateT x tok.accessToken) (auth, man)

liftEitherSpot :: (MonadSpotify m) => Either ClientError a -> m a
liftEitherSpot = either throwClientError pure

inSpot :: forall m a. (MonadSpotify m) => (AccessToken -> ClientM a) -> m a
inSpot x = do
    tok <- getToken
    man <- getManager
    liftIO (runClientM (x tok) $ mkClientEnv man mainBase) >>= \case
        Left e ->
            expiry e >>= \case
                True -> retry
                False -> throwClientError e
        Right r -> pure r
  where
    -- get a new token and try again
    retry = do
        putToken . (.accessToken) =<< newToken
        inSpot x
    -- does the error indicate that the access token has expired?
    expiry = \case
        FailureResponse _ resp -> do
            if statusCode (responseStatusCode resp) == 401
                then do
                    Error{message} <- liftEitherSpot $ bimap mkError (.error) $ eitherDecode @Error' $ responseBody resp
                    if message == "The access token expired"
                        then pure True
                        else no
                else no
          where
            mkError s = DecodeFailure ("Failed to decode a spotify error: " <> T.pack s) resp
        _ -> no
      where
        no = pure False
newtype Error' = Error' {error :: Error} -- internal - used for decoding the errors we get from Spotify responses
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON)

data Auth = Auth
    { refreshToken :: RefreshToken
    , clientId :: ClientId
    , clientSecret :: ClientSecret
    }
    deriving (Show)

mainBase, accountsBase :: BaseUrl
mainBase = BaseUrl Https "api.spotify.com" 443 "v1"
accountsBase = BaseUrl Https "accounts.spotify.com" 443 "api"

-- helpers for wrapping Servant API
noContent :: (Functor f) => f NoContent -> f ()
noContent = fmap \NoContent -> ()
marketFromToken :: Maybe Market
marketFromToken = Just "from_token"
withPagingParams :: PagingParams -> (Maybe Int -> Maybe Int -> t) -> t
withPagingParams PagingParams{limit, offset} f = f limit offset

data PagingParams = PagingParams
    { limit :: Maybe Int
    , offset :: Maybe Int
    }
noPagingParams :: PagingParams
noPagingParams = PagingParams Nothing Nothing

newToken :: (MonadSpotify m) => m TokenResponse
newToken = liftEitherSpot =<< liftIO =<< (newTokenIO <$> getAuth <*> getManager)
newTokenIO :: Auth -> Manager -> IO (Either ClientError TokenResponse)
newTokenIO a m = runClientM (requestToken a) (mkClientEnv m accountsBase)
  where
    requestToken (Auth t i s) =
        Spotify.Client.refreshAccessToken
            (RefreshAccessTokenForm t)
            (IdAndSecret i s)
newTokenIO' :: (MonadIO m) => Manager -> ClientId -> ClientSecret -> URL -> AuthCode -> m (Either ClientError TokenResponse')
newTokenIO' man clientId clientSecret redirectURI authCode =
    liftIO $
        runClientM
            ( Spotify.Client.requestAccessToken
                (RequestAccessTokenForm authCode redirectURI)
                (IdAndSecret clientId clientSecret)
            )
            (mkClientEnv man accountsBase)

-- spotipy-esque
getAuthCodeInteractive :: ClientId -> URL -> Maybe (Set Scope) -> IO (Maybe AuthCode)
getAuthCodeInteractive clientId redirectURI scopes = do
    T.putStrLn $ "Go to this URL: " <> (authorizeUrl clientId redirectURI scopes).unwrap
    T.putStr "Copy the URL you are redirected to: " >> hFlush stdout
    fmap AuthCode . T.stripPrefix (redirectURI.unwrap <> "/?code=") <$> T.getLine
authorizeUrl :: ClientId -> URL -> Maybe (Set Scope) -> URL
authorizeUrl clientId redirectURI scopes =
    URL $
        "https://"
            <> T.pack
                ( baseUrlHost accountsBase
                    <> "/"
                    <> show (linkURI link)
                )
  where
    link =
        link0
            clientId
            "code"
            redirectURI
            Nothing
            (ScopeSet <$> scopes)
            Nothing
    _ :<|> _ :<|> link0 = allLinks $ Proxy @Spotify.Servant.AccountsAPI

flip0 :: (a0 -> b) -> a0 -> b
flip0 f = f
flip1 :: (a0 -> a1 -> b) -> a1 -> a0 -> b
flip1 f = flip0 . flip f
flip2 :: (a0 -> a1 -> a2 -> b) -> a1 -> a2 -> a0 -> b
flip2 f = flip1 . flip f
flip3 :: (a0 -> a1 -> a2 -> a3 -> b) -> a1 -> a2 -> a3 -> a0 -> b
flip3 f = flip2 . flip f
flip4 :: (a0 -> a1 -> a2 -> a3 -> a4 -> b) -> a1 -> a2 -> a3 -> a4 -> a0 -> b
flip4 f = flip3 . flip f

getAlbum :: (MonadSpotify m) => AlbumID -> m Album
getAlbum a = inSpot $ flip2 Spotify.Client.getAlbum a marketFromToken
getAlbumTracks :: (MonadSpotify m) => AlbumID -> PagingParams -> m (Paging TrackSimple)
getAlbumTracks a pps = inSpot $ withPagingParams pps . flip2 Spotify.Client.getAlbumTracks a marketFromToken
removeAlbums :: (MonadSpotify m) => [AlbumID] -> m ()
removeAlbums = noContent . inSpot . flip1 Spotify.Client.removeAlbums . IDs

getArtist :: (MonadSpotify m) => ArtistID -> m Artist
getArtist = inSpot . flip1 Spotify.Client.getArtist

getTrack :: (MonadSpotify m) => TrackID -> m Track
getTrack t = inSpot $ flip2 Spotify.Client.getTrack t marketFromToken
getSavedTracks :: (MonadSpotify m) => PagingParams -> m (Paging SavedTrack)
getSavedTracks pps = inSpot $ withPagingParams pps . flip1 Spotify.Client.getSavedTracks marketFromToken
saveTracks :: (MonadSpotify m) => [TrackID] -> m ()
saveTracks = noContent . inSpot . flip1 Spotify.Client.saveTracks . IDs
removeTracks :: (MonadSpotify m) => [TrackID] -> m ()
removeTracks = noContent . inSpot . flip1 Spotify.Client.removeTracks . IDs

search :: (MonadSpotify m) => Text -> [SearchType] -> Maybe Text -> Maybe Market -> PagingParams -> m SearchResult
search q t e m = inSpot . flip (flip withPagingParams . flip4 Spotify.Client.getSearch q t e m)

getMe :: (MonadSpotify m) => m User
getMe = inSpot $ flip0 Spotify.Client.getMe
getUser :: (MonadSpotify m) => UserID -> m User
getUser u = inSpot $ flip1 Spotify.Client.getUser u
unfollowPlaylist :: (MonadSpotify m) => PlaylistID -> m ()
unfollowPlaylist = noContent . inSpot . flip1 Spotify.Client.unfollowPlaylist

getPlaylist :: (MonadSpotify m) => PlaylistID -> m Playlist
getPlaylist = inSpot . flip1 Spotify.Client.getPlaylist
addToPlaylist :: (MonadSpotify m) => PlaylistID -> Maybe Int -> [URI] -> m Text
addToPlaylist p position uris = fmap coerce $ inSpot $ flip2 Spotify.Client.addToPlaylist p AddToPlaylistBody{..}
getMyPlaylists :: (MonadSpotify m) => PagingParams -> m (Paging PlaylistSimple)
getMyPlaylists pps = inSpot $ withPagingParams pps . flip0 Spotify.Client.getMyPlaylists
createPlaylist :: (MonadSpotify m) => UserID -> CreatePlaylistOpts -> m PlaylistSimple
createPlaylist u opts = inSpot $ flip2 Spotify.Client.createPlaylist u opts

getCategories :: (MonadSpotify m) => CategoryID -> Maybe Country -> Maybe Locale -> m Category
getCategories = inSpot .:. flip3 Spotify.Client.getCategories

getEpisode :: (MonadSpotify m) => EpisodeID -> m Episode
getEpisode e = inSpot $ flip2 Spotify.Client.getEpisode e marketFromToken
getSavedEpisodes :: (MonadSpotify m) => PagingParams -> m (Paging SavedEpisode)
getSavedEpisodes pps = inSpot $ withPagingParams pps $ flip3 Spotify.Client.getSavedEpisodes marketFromToken
saveEpisodes :: (MonadSpotify m) => [EpisodeID] -> m ()
saveEpisodes = noContent . inSpot . flip1 Spotify.Client.saveEpisodes . IDs
removeEpisodes :: (MonadSpotify m) => [EpisodeID] -> m ()
removeEpisodes = noContent . inSpot . flip1 Spotify.Client.removeEpisodes . IDs

getPlaybackState :: (MonadSpotify m) => m (Maybe PlaybackState)
getPlaybackState = fmap handleAllJSONOrNoContent $ inSpot $ flip2 Spotify.Client.getPlaybackState marketFromToken $ Just "episode"
transferPlayback :: (MonadSpotify m) => [DeviceID] -> Bool -> m ()
transferPlayback device_ids play = noContent . inSpot $ flip1 Spotify.Client.transferPlayback TransferPlaybackBody{..}
getAvailableDevices :: (MonadSpotify m) => m [Device]
getAvailableDevices = fmap (.devices) . inSpot $ flip0 Spotify.Client.getAvailableDevices
getCurrentlyPlayingTrack :: (MonadSpotify m) => m CurrentlyPlayingTrack
getCurrentlyPlayingTrack = inSpot $ flip1 Spotify.Client.getCurrentlyPlayingTrack marketFromToken
startPlayback :: (MonadSpotify m) => Maybe DeviceID -> StartPlaybackOpts -> m ()
startPlayback = noContent . inSpot .: flip2 Spotify.Client.startPlayback
pausePlayback :: (MonadSpotify m) => Maybe DeviceID -> m ()
pausePlayback = noContent . inSpot . flip1 Spotify.Client.pausePlayback
skipToNext :: (MonadSpotify m) => Maybe DeviceID -> m ()
skipToNext = noContent . inSpot . flip1 Spotify.Client.skipToNext
skipToPrevious :: (MonadSpotify m) => Maybe DeviceID -> m ()
skipToPrevious = noContent . inSpot . flip1 Spotify.Client.skipToPrevious
seekToPosition :: (MonadSpotify m) => Int -> Maybe DeviceID -> m ()
seekToPosition = noContent . inSpot .: flip2 Spotify.Client.seekToPosition

-- higher-level wrappers around main API
-- takes a callback which can be used for side effects, or to return False for early exit
allPages :: (Monad m) => Maybe (Paging a -> m Bool) -> (PagingParams -> m (Paging a)) -> m [a]
allPages callback x =
    concat <$> flip unfoldrM (0, Nothing, True) \(i, total, keepGoing) -> do
        if keepGoing && maybe True (i <) total
            then do
                p <- x $ PagingParams{limit = Just limit, offset = Just i}
                keepGoing' <- maybe (pure True) ($ p) callback
                pure $ Just (p.items, (i + limit, Just p.total, keepGoing'))
            else pure Nothing
  where
    limit = 50 -- API docs say this is the max
