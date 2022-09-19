module Spotify where

import Spotify.Servant.Albums
import Spotify.Servant.Artists
import Spotify.Servant.Categories
import Spotify.Servant.Core
import Spotify.Servant.Player
import Spotify.Servant.Playlists
import Spotify.Servant.Tracks
import Spotify.Servant.Users
import Spotify.Types.Albums
import Spotify.Types.Artists
import Spotify.Types.Auth
import Spotify.Types.Categories
import Spotify.Types.Misc
import Spotify.Types.Player
import Spotify.Types.Playlists
import Spotify.Types.Simple
import Spotify.Types.Tracks
import Spotify.Types.Users

import Control.Applicative ((<|>))
import Control.Exception (throwIO)
import Control.Monad.Except (ExceptT, MonadError, liftEither, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Loops (unfoldrM)
import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)
import Control.Monad.State (MonadState, StateT, get, put, runStateT)
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
import Servant.API (NoContent (NoContent))
import Servant.Client (BaseUrl (BaseUrl, baseUrlHost), ClientError (DecodeFailure, FailureResponse), ClientM, HasClient (Client), Scheme (Http), client, mkClientEnv, responseBody, responseStatusCode, runClientM)
import Servant.Links (allLinks, linkURI)
import System.Directory (XdgDirectory (XdgConfig), createDirectoryIfMissing, getTemporaryDirectory, getXdgDirectory)
import System.FilePath ((</>))
import System.IO (hFlush, stdout)

class MonadIO m => MonadSpotify m where
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
                    res <- T.putStr (prompt <> ": ") >> T.getLine
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
runSpotify = fmap (fmap fst) .: runSpotify' Nothing Nothing
runSpotify' :: Maybe Manager -> Maybe AccessToken -> Auth -> Spotify a -> IO (Either ClientError (a, AccessToken))
runSpotify' mm mt a x = do
    man <- maybe newTlsManager pure mm
    let tok = maybe (fmap (.accessToken) . liftEither =<< liftIO (newTokenIO a man)) pure mt
    runExceptT $ runReaderT (runStateT x.unwrap =<< tok) (a, man)

liftEitherSpot :: MonadSpotify m => Either ClientError a -> m a
liftEitherSpot = either throwClientError pure

inSpot :: forall m a. MonadSpotify m => (AccessToken -> ClientM a) -> m a
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
cli :: forall api. HasClient ClientM api => Client ClientM api
cli = client $ Proxy @api
noContent :: Functor f => f NoContent -> f ()
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

newToken :: MonadSpotify m => m TokenResponse
newToken = liftEitherSpot =<< liftIO =<< (newTokenIO <$> getAuth <*> getManager)
newTokenIO :: Auth -> Manager -> IO (Either ClientError TokenResponse)
newTokenIO a m = runClientM (requestToken a) (mkClientEnv m accountsBase)
  where
    requestToken (Auth t i s) =
        cli @RefreshAccessToken
            (RefreshAccessTokenForm t)
            (IdAndSecret i s)
newTokenIO' :: MonadIO m => Manager -> ClientId -> ClientSecret -> URL -> AuthCode -> m (Either ClientError TokenResponse')
newTokenIO' man clientId clientSecret redirectURI authCode =
    liftIO $
        runClientM
            ( cli @RequestAccessToken
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
        allLinks
            (Proxy @Authorize)
            clientId
            "code"
            redirectURI
            Nothing
            (ScopeSet <$> scopes)
            Nothing

getAlbum :: MonadSpotify m => AlbumID -> m Album
getAlbum a = inSpot $ cli @GetAlbum a marketFromToken
getAlbumTracks :: MonadSpotify m => AlbumID -> PagingParams -> m (Paging TrackSimple)
getAlbumTracks a pps = inSpot $ withPagingParams pps $ cli @GetAlbumTracks a marketFromToken
removeAlbums :: MonadSpotify m => [AlbumID] -> m ()
removeAlbums = noContent . inSpot . cli @RemoveAlbums

getArtist :: MonadSpotify m => ArtistID -> m Artist
getArtist = inSpot . cli @GetArtist

getTrack :: MonadSpotify m => TrackID -> m Track
getTrack t = inSpot $ cli @GetTrack t marketFromToken
getSavedTracks :: MonadSpotify m => PagingParams -> m (Paging SavedTrack)
getSavedTracks pps = inSpot $ withPagingParams pps $ cli @GetSavedTracks marketFromToken
saveTracks :: MonadSpotify f => [TrackID] -> f ()
saveTracks = noContent . inSpot . cli @SaveTracks
removeTracks :: MonadSpotify f => [TrackID] -> f ()
removeTracks = noContent . inSpot . cli @RemoveTracks

getMe :: MonadSpotify m => m User
getMe = inSpot $ cli @GetMe
getUser :: MonadSpotify m => UserID -> m User
getUser u = inSpot $ cli @GetUser u
unfollowPlaylist :: MonadSpotify m => PlaylistID -> m ()
unfollowPlaylist = noContent . inSpot . cli @UnfollowPlaylist

getPlaylist :: MonadSpotify m => PlaylistID -> m Playlist
getPlaylist = inSpot . cli @GetPlaylist
addToPlaylist :: MonadSpotify m => PlaylistID -> Maybe Int -> [URI] -> m Text
addToPlaylist p position uris = fmap coerce $ inSpot $ cli @AddToPlaylist p AddToPlaylistBody{..}
getMyPlaylists :: MonadSpotify m => PagingParams -> m (Paging PlaylistSimple)
getMyPlaylists pps = inSpot $ withPagingParams pps $ cli @GetMyPlaylists

createPlaylist :: MonadSpotify m => UserID -> CreatePlaylistOpts -> m PlaylistSimple
createPlaylist u opts = inSpot $ cli @CreatePlaylist u opts

getCategories :: MonadSpotify m => CategoryID -> Maybe Country -> Maybe Locale -> m Category
getCategories = inSpot .:. cli @GetCategories

getPlaybackState :: MonadSpotify m => Maybe Market -> m PlaybackState
getPlaybackState = inSpot . cli @GetPlaybackState
transferPlayback :: MonadSpotify m => [DeviceID] -> Bool -> m ()
transferPlayback device_ids play = noContent . inSpot $ cli @TransferPlayback TransferPlaybackBody{..}
getAvailableDevices :: MonadSpotify m => m [Device]
getAvailableDevices = fmap (.devices) . inSpot $ cli @GetAvailableDevices
getCurrentlyPlayingTrack :: MonadSpotify m => Maybe Market -> m CurrentlyPlayingTrack
getCurrentlyPlayingTrack = inSpot . cli @GetCurrentlyPlayingTrack
startPlayback :: MonadSpotify m => Maybe DeviceID -> StartPlaybackOpts -> m ()
startPlayback = noContent . inSpot .: cli @StartPlayback
pausePlayback :: MonadSpotify m => Maybe DeviceID -> m ()
pausePlayback = noContent . inSpot . cli @PausePlayback
skipToNext :: MonadSpotify m => Maybe DeviceID -> m ()
skipToNext = noContent . inSpot . cli @SkipToNext
skipToPrevious :: MonadSpotify m => Maybe DeviceID -> m ()
skipToPrevious = noContent . inSpot . cli @SkipToPrevious
seekToPosition :: MonadSpotify m => Int -> Maybe DeviceID -> m ()
seekToPosition = noContent . inSpot .: cli @SeekToPosition

-- higher-level wrappers around main API
-- takes a callback which can be used for side effects, or to return False for early exit
allPages :: Monad m => Maybe (Paging a -> m Bool) -> (PagingParams -> m (Paging a)) -> m [a]
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
