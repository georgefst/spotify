{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Spotify where

import Spotify.Servant.Albums
import Spotify.Servant.Artists
import Spotify.Servant.Categories
import Spotify.Servant.Core
import Spotify.Servant.Episodes
import Spotify.Servant.Player
import Spotify.Servant.Playlists
import Spotify.Servant.Search
import Spotify.Servant.Tracks
import Spotify.Servant.Users
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
import Servant.API (NoContent (NoContent), (:<|>) ((:<|>)), (:>))
import Servant.API.Flatten (flatten)
import Servant.Client (BaseUrl (BaseUrl, baseUrlHost), ClientError (DecodeFailure, FailureResponse), ClientM, Scheme (Http), client, mkClientEnv, responseBody, responseStatusCode, runClientM)
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
mainBase = BaseUrl Http "api.spotify.com" 80 "v1"
accountsBase = BaseUrl Http "accounts.spotify.com" 80 "api"

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
        refreshAccessToken0
            (RefreshAccessTokenForm t)
            (IdAndSecret i s)
newTokenIO' :: (MonadIO m) => Manager -> ClientId -> ClientSecret -> URL -> AuthCode -> m (Either ClientError TokenResponse')
newTokenIO' man clientId clientSecret redirectURI authCode =
    liftIO $
        runClientM
            ( requestAccessToken0
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
    _ :<|> _ :<|> link0 = allLinks $ Proxy @AccountsAPI

refreshAccessToken0
    :<|> requestAccessToken0
    :<|> authorize0 =
        client $ Proxy @AccountsAPI

type AccountsAPI =
    RefreshAccessToken
        :<|> RequestAccessToken
        :<|> Authorize

getAlbum0
    :<|> getAlbumTracks0
    :<|> removeAlbums0
    :<|> getArtist0
    :<|> getCategories0
    :<|> getEpisode0
    :<|> getSavedEpisodes0
    :<|> saveEpisodes0
    :<|> removeEpisodes0
    :<|> getPlaybackState0
    :<|> transferPlayback0
    :<|> getAvailableDevices0
    :<|> getCurrentlyPlayingTrack0
    :<|> startPlayback0
    :<|> pausePlayback0
    :<|> skipToNext0
    :<|> skipToPrevious0
    :<|> seekToPosition0
    :<|> getPlaylist0
    :<|> addToPlaylist0
    :<|> getMyPlaylists0
    :<|> createPlaylist0
    :<|> getSearch0
    :<|> getTrack0
    :<|> getSavedTracks0
    :<|> saveTracks0
    :<|> removeTracks0
    :<|> getMe0
    :<|> getUser0
    :<|> unfollowPlaylist0 =
        client (flatten $ Proxy @MainAPI)

type MainAPI =
    AuthHeader
        :> ( GetAlbum
                :<|> GetAlbumTracks
                :<|> RemoveAlbums
                :<|> GetArtist
                :<|> GetCategories
                :<|> GetEpisode
                :<|> GetSavedEpisodes
                :<|> SaveEpisodes
                :<|> RemoveEpisodes
                :<|> GetPlaybackState
                :<|> TransferPlayback
                :<|> GetAvailableDevices
                :<|> GetCurrentlyPlayingTrack
                :<|> StartPlayback
                :<|> PausePlayback
                :<|> SkipToNext
                :<|> SkipToPrevious
                :<|> SeekToPosition
                :<|> GetPlaylist
                :<|> AddToPlaylist
                :<|> GetMyPlaylists
                :<|> CreatePlaylist
                :<|> GetSearch
                :<|> GetTrack
                :<|> GetSavedTracks
                :<|> SaveTracks
                :<|> RemoveTracks
                :<|> GetMe
                :<|> GetUser
                :<|> UnfollowPlaylist
           )

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
flip5 :: (a0 -> a1 -> a2 -> a3 -> a4 -> a5 -> b) -> a1 -> a2 -> a3 -> a4 -> a5 -> a0 -> b
flip5 f = flip4 . flip f
flip6 :: (a0 -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> b) -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a0 -> b
flip6 f = flip5 . flip f

getAlbum :: (MonadSpotify m) => AlbumID -> m Album
getAlbum a = inSpot $ flip2 getAlbum0 a marketFromToken
getAlbumTracks :: (MonadSpotify m) => AlbumID -> PagingParams -> m (Paging TrackSimple)
getAlbumTracks a pps = inSpot $ withPagingParams pps . flip2 getAlbumTracks0 a marketFromToken
removeAlbums :: (MonadSpotify m) => [AlbumID] -> m ()
removeAlbums = noContent . inSpot . flip1 removeAlbums0 . SpotIDs

getArtist :: (MonadSpotify m) => ArtistID -> m Artist
getArtist = inSpot . flip1 getArtist0

getTrack :: (MonadSpotify m) => TrackID -> m Track
getTrack t = inSpot $ flip2 getTrack0 t marketFromToken
getSavedTracks :: (MonadSpotify m) => PagingParams -> m (Paging SavedTrack)
getSavedTracks pps = inSpot $ withPagingParams pps . flip1 getSavedTracks0 marketFromToken
saveTracks :: (MonadSpotify m) => [TrackID] -> m ()
saveTracks = noContent . inSpot . flip1 saveTracks0 . SpotIDs
removeTracks :: (MonadSpotify m) => [TrackID] -> m ()
removeTracks = noContent . inSpot . flip1 removeTracks0 . SpotIDs

search :: (MonadSpotify m) => Text -> [SearchType] -> Maybe Text -> Maybe Market -> PagingParams -> m SearchResult
search q t e m = inSpot . flip withPagingParams \limit offset -> flip6 getSearch0 q t e limit m offset

getMe :: (MonadSpotify m) => m User
getMe = inSpot $ flip0 getMe0
getUser :: (MonadSpotify m) => UserID -> m User
getUser u = inSpot $ flip1 getUser0 u
unfollowPlaylist :: (MonadSpotify m) => PlaylistID -> m ()
unfollowPlaylist = noContent . inSpot . flip1 unfollowPlaylist0

getPlaylist :: (MonadSpotify m) => PlaylistID -> m Playlist
getPlaylist = inSpot . flip1 getPlaylist0
addToPlaylist :: (MonadSpotify m) => PlaylistID -> Maybe Int -> [URI] -> m Text
addToPlaylist p position uris = fmap coerce $ inSpot $ flip2 addToPlaylist0 p AddToPlaylistBody{..}
getMyPlaylists :: (MonadSpotify m) => PagingParams -> m (Paging PlaylistSimple)
getMyPlaylists pps = inSpot $ withPagingParams pps . flip0 getMyPlaylists0
createPlaylist :: (MonadSpotify m) => UserID -> CreatePlaylistOpts -> m PlaylistSimple
createPlaylist u opts = inSpot $ flip2 createPlaylist0 u opts

getCategories :: (MonadSpotify m) => CategoryID -> Maybe Country -> Maybe Locale -> m Category
getCategories = inSpot .:. flip3 getCategories0

getEpisode :: (MonadSpotify m) => EpisodeID -> m Episode
getEpisode e = inSpot $ flip2 getEpisode0 e marketFromToken
getSavedEpisodes :: (MonadSpotify m) => PagingParams -> m (Paging SavedEpisode)
getSavedEpisodes pps = inSpot $ withPagingParams pps $ flip3 getSavedEpisodes0 marketFromToken
saveEpisodes :: (MonadSpotify m) => [EpisodeID] -> m ()
saveEpisodes = noContent . inSpot . flip1 saveEpisodes0 . SpotIDs
removeEpisodes :: (MonadSpotify m) => [EpisodeID] -> m ()
removeEpisodes = noContent . inSpot . flip1 removeEpisodes0 . SpotIDs

getPlaybackState :: (MonadSpotify m) => m (Maybe PlaybackState)
getPlaybackState = fmap handleAllJSONOrNoContent $ inSpot $ flip2 getPlaybackState0 marketFromToken $ Just "episode"
transferPlayback :: (MonadSpotify m) => [DeviceID] -> Bool -> m ()
transferPlayback device_ids play = noContent . inSpot $ flip1 transferPlayback0 TransferPlaybackBody{..}
getAvailableDevices :: (MonadSpotify m) => m [Device]
getAvailableDevices = fmap (.devices) . inSpot $ flip0 getAvailableDevices0
getCurrentlyPlayingTrack :: (MonadSpotify m) => m CurrentlyPlayingTrack
getCurrentlyPlayingTrack = inSpot $ flip1 getCurrentlyPlayingTrack0 marketFromToken
startPlayback :: (MonadSpotify m) => Maybe DeviceID -> StartPlaybackOpts -> m ()
startPlayback = noContent . inSpot .: flip2 startPlayback0
pausePlayback :: (MonadSpotify m) => Maybe DeviceID -> m ()
pausePlayback = noContent . inSpot . flip1 pausePlayback0
skipToNext :: (MonadSpotify m) => Maybe DeviceID -> m ()
skipToNext = noContent . inSpot . flip1 skipToNext0
skipToPrevious :: (MonadSpotify m) => Maybe DeviceID -> m ()
skipToPrevious = noContent . inSpot . flip1 skipToPrevious0
seekToPosition :: (MonadSpotify m) => Int -> Maybe DeviceID -> m ()
seekToPosition = noContent . inSpot .: flip2 seekToPosition0

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
