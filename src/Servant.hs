{-# LANGUAGE AllowAmbiguousTypes #-}
-- https://developer.spotify.com/documentation/web-api/reference/
    -- (at time of writing (May 2020) the beta documentation suggested is crap)
{-# OPTIONS_GHC -Wno-orphans #-}

module Servant where --TODO explicit export list

-- we hide 'error' to ease deriving the FromJSON instance for Error' (who wants errors, anyway...)
import Prelude hiding (error)

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor
import Data.Composition
import Data.Generics.Labels
import Data.Maybe
import Data.Monoid
import Data.Proxy
import GHC.TypeLits
import Lens.Micro.Extras
import Network.HTTP.Client (Manager,newManager)
import Network.HTTP.Client.TLS
import Network.HTTP.Types (Status(statusCode))
import Servant.API
import Servant.Client
import System.Directory
import System.FilePath
import Text.Pretty.Simple

import qualified Data.Aeson as JSON
import Data.Aeson (FromJSON(..),ToJSON(..))
import Data.Aeson.Types (Parser)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as SF
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as H
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import GHC.Generics (Generic)

import Types --TODO special naming for fields that clash with prelude? eg. 'id'


{- The Spotify monad, and MonadSpotify type class -}

--TODO do we really want to expose ClientError directly?
    -- pros
        -- no error info lost
    -- cons
        -- requires user to care about servant
        -- bespoke type would allow us to be more specific about particular spotify errors
    -- we ought to return the spotify Error object when one is returned
class MonadIO m => MonadSpotify m where
    getAuth :: m Auth
    getManager :: m Manager
    getToken :: m Token
    putToken :: Token -> m ()
    throwClientError :: ClientError -> m a
    throwClientError = liftIO . throwIO --TODO document default behaviour

--TODO document this fully, with caveat about writing secret and token to disk etc.
-- useful for REPL use etc.
instance MonadSpotify IO where
    getAuth = do
        dir <- getXdgDirectory XdgData "spotify-web-haskell"
        let getData :: FilePath -> Text -> IO Text -- look for file - otherwise get from stdin
            getData file prompt = T.readFile path <|> do
                res <- T.putStr (prompt <> ": ") >> T.getLine
                createDirectoryIfMissing False dir
                T.writeFile path res
                return res
                where path = dir </> file
        Auth
            <$> (RefreshToken                <$> getData "refresh" "Refresh token")
            <*> (ClientId     . T.encodeUtf8 <$> getData "id"      "Client id"    )
            <*> (ClientSecret . T.encodeUtf8 <$> getData "secret"  "Client secret")
    getManager = newManager tlsManagerSettings
    getToken = do
        path <- tokenPath
        Token <$> T.readFile path <|> do
            tok <- newToken
            putToken tok
            return tok
    putToken (Token t) = do
        path <- tokenPath
        T.writeFile path t
tokenPath :: IO FilePath
tokenPath = (</> "spotify-haskell-token") <$> getTemporaryDirectory

newtype Spotify a = Spotify {
    unSpot :: StateT Token (ReaderT (Auth, Manager) (ExceptT ClientError IO)) a}
    deriving newtype (Functor, Applicative, Monad, MonadIO,
        MonadState Token, MonadReader (Auth, Manager), MonadError ClientError)

instance MonadSpotify Spotify where
    getAuth = asks fst
    getManager = asks snd
    getToken = get
    putToken = put
    throwClientError = throwError

--TODO does 'runClientM' guarantee that no other types of exception are thrown?
runSpotify :: Maybe Manager -> Maybe Token -> Auth -> Spotify a -> IO (Either ClientError (a, Token))
runSpotify mm mt a x = do
    man <- maybe (newManager tlsManagerSettings) return mm
    let getTok = liftEither =<< liftIO (newTokenIO a man)
        rdr = runStateT (unSpot x) =<< maybe getTok return mt
    runExceptT $ runReaderT rdr (a, man)


{- Exposed types -}

-- client authorization data
data Auth = Auth
    { refreshToken :: RefreshToken
    , clientId :: ClientId
    , clientSecret :: ClientSecret
    }

newtype RefreshToken = RefreshToken Text
    deriving (Eq,Ord,Show)
newtype ClientId = ClientId ByteString
    deriving (Eq,Ord,Show)
newtype ClientSecret = ClientSecret ByteString
    deriving (Eq,Ord,Show)


{- Internal helper types -}

--TODO no longer internal
newtype Token = Token Text deriving Show
-- TODO do we ever want anything other than 'Bearer'?
instance ToHttpApiData Token where
    toUrlPiece (Token t) = toUrlPiece $ "Bearer " <> t

data IdAndSecret = IdAndSecret ClientId ClientSecret
instance ToHttpApiData IdAndSecret where
    toUrlPiece (IdAndSecret (ClientId i) (ClientSecret s)) =
        toUrlPiece . ("Basic " <>) . T.decodeUtf8 . SF.encode $ i <> ":" <> s

type AuthHeader = Header' '[Strict,Required] "Authorization" Token

type AuthHeaderBasic = Header' '[Strict,Required] "Authorization" IdAndSecret

-- this aligns with the errors we get from Spotify via Servant
newtype Error' = Error' { error :: Error }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass FromJSON

--TODO separate into Map and Snd (or import library)
type family MapSnd (js :: [(k1,k2)]) :: [k2] where
    MapSnd '[] = '[]
    MapSnd ('(_,j) : js) = j : MapSnd js

type family JT (js :: [*]) where
    JT '[] = ()
    JT (j : js) = (j, JT js)

class JMap (js :: [(Symbol, Type)]) where
    data JRes js
    jRes :: JRes js -> JT (MapSnd js)
    parseJSON' :: JSON.Object -> Parser (JRes js)
    type JFunc js r
    jFunc :: (JRes js -> r) -> JFunc js r
    toJSON' :: JRes js -> [(Text, JSON.Value)]
instance JMap '[] where
    data JRes '[] = J0
    type JFunc '[] r = r
    toJSON' J0 = []
    jRes J0 = ()
    jFunc f = f J0
    parseJSON' _ = return J0
instance (JMap js, KnownSymbol s, FromJSON a, ToJSON a) => JMap ('(s,a) : js) where
    data JRes ('(s,a) : js) = Jn a (JRes js)
    parseJSON' o = do
        x <- maybe (fail $ "key not found in JSON object: " ++ sym) return (H.lookup (T.pack sym) o)
        Jn <$> parseJSON x <*> parseJSON' o
        where sym = symbolVal $ Proxy @s
    toJSON' (Jn x xs) = (T.pack $ symbolVal $ Proxy @s, toJSON x) : toJSON' xs
    jRes (Jn x xs) = (x, jRes xs)
    type JFunc ('(s,a) : js) r = a -> JFunc js r
    jFunc f y = jFunc $ f . Jn y

instance JMap js => FromJSON (JRes js) where
    parseJSON = \case
        JSON.Object o -> parseJSON' o
        v -> fail $ "JSON is not an object: " ++ show v

instance JMap js => ToJSON (JRes js) where
    toJSON = JSON.Object . H.fromList . toJSON'

newtype Market = Market Text
    deriving newtype ToHttpApiData
marketFromToken :: (Maybe Market -> b) -> b
marketFromToken = ($ Just $ Market "from_token")


{- Stuff that makes writing all the bindings less tedious, but that I might remove in the long run -}

type GetS a = AuthHeader :> Get '[JSON] a
type PutS a = AuthHeader :> Put '[JSON] a
type PostS a = AuthHeader :> Post '[JSON] a
type DeleteS a = AuthHeader :> Delete '[JSON] a

type BodyS = ReqBody '[JSON]

client' :: forall api. HasClient ClientM api => Client ClientM api
client' = client $ Proxy @api

noContent :: NoContent -> ()
noContent NoContent = ()

tuple1 :: (a, ()) -> a
tuple1 (a,()) = a


{- Albums -}

--TODO what does the label "id" actually do?
-- getAlbum :: MonadSpotify m => Text -> Text -> m Album
getAlbum :: MonadSpotify m => Text -> m Album
getAlbum = marketFromToken $ inSpot .: client' @(
    QueryParam "market" Market :> "albums" :> Capture "id" Text :> GetS Album )

getAlbumTracks :: MonadSpotify m => Text -> m (Paging TrackSimplified)
getAlbumTracks = inSpot . client' @("albums" :> Capture "id" Text :> "tracks" :> GetS (Paging TrackSimplified))


{- Follow -}

-- note that Spotify has no notion of actually 'deleting' a playlist - this is closest
unfollowPlaylist :: MonadSpotify m => Text -> m NoContent
unfollowPlaylist = inSpot . client' @(
    "playlists" :> Capture "playlist_id" Text :> "followers" :> DeleteS NoContent )


{- Library -}

--TODO query params
getSavedTracks :: MonadSpotify m => m (Paging SavedTrack)
getSavedTracks = inSpot $ client' @("me" :> "tracks" :> GetS (Paging SavedTrack))

removeAlbums :: MonadSpotify f => [Text] -> f ()
removeAlbums = fmap noContent . inSpot . client' @("me" :> "albums" :> BodyS [Text] :> DeleteS NoContent)

removeTracks :: MonadSpotify f => [Text] -> f ()
removeTracks = fmap noContent . inSpot . client' @("me" :> "tracks" :> BodyS [Text] :> DeleteS NoContent)

--TODO fails silently - even through Spotify Web Console
--TODO take a NonEmpty? and for similar endpoints
saveTracks :: MonadSpotify f => [Text] -> f ()
saveTracks = fmap noContent . inSpot . client' @("me" :> "tracks" :> BodyS [Text] :> DeleteS NoContent)


{- Playlists -}

--TODO don't export directly
addToPlaylist :: MonadSpotify f => [Text] -> Maybe Int -> Text -> f Text
addToPlaylist = jFunc $ fmap (tuple1 . jRes) . inSpot .: client' @(
    BodyS (JRes '[ '("uris", [Text]), '("position", Maybe Int)]) :>
    "playlists" :> Capture "playlist_id" Text :> "tracks" :>
    PostS (JRes '[ '("snapshot_id", Text)]) )
addTrackToPlaylist :: MonadSpotify f => [Text] -> Maybe Int -> Text -> f Text
addTrackToPlaylist = addToPlaylist . map ("spotify:track:" <>)
addEpisodeToPlaylist :: MonadSpotify f => [Text] -> Maybe Int -> Text -> f Text
addEpisodeToPlaylist = addToPlaylist . map ("spotify:episode:" <>)

--TODO we have to manually pass in our own username here,
        -- even though we seemingly never have permission to add a playlist for someone else
    -- store username in Reader env?
--TODO non-simple playlist doesn't seem to exist
createPlaylist :: MonadSpotify m => Text -> Bool -> Bool -> Text -> Text -> m PlaylistSimplified
createPlaylist = jFunc $ inSpot .: client' @(
    BodyS (JRes '[ '("name", Text), '("public", Bool), '("collaborative", Bool), '("description", Text) ]) :>
    "users" :> Capture "user_id" Text :> "playlists" :>
    PostS PlaylistSimplified )

getMyPlaylists :: MonadSpotify m => Maybe Int -> Maybe Int -> m (Paging PlaylistSimplified)
getMyPlaylists = inSpot .: client' @(
    "me" :> "playlists" :>
    QueryParam "limit" Int :> QueryParam "offset" Int :>
    GetS (Paging PlaylistSimplified) )


{- Tracks -}

getTrack :: MonadSpotify m => Text -> m Track
getTrack = inSpot . client' @("tracks" :> Capture "id" Text :> GetS Track)


{- Users -}

getMe :: MonadSpotify m => m UserPublic
getMe = inSpot $ client' @("me" :> GetS UserPublic)

getUser :: MonadSpotify m => Text -> m UserPublic
getUser = inSpot . client' @("users" :> Capture "user_id" Text :> GetS UserPublic)


{- Auth -}

type AuthAPI = "token" :> ReqBody '[FormUrlEncoded] [(Text, Text)] :> AuthHeaderBasic :> Post '[JSON] TokenResponse


{- Constants -}

mainBase, accountsBase :: BaseUrl
mainBase = BaseUrl Http "api.spotify.com" 80 "v1"
accountsBase = BaseUrl Http "accounts.spotify.com" 80 "api"


{- Helpers -}

inSpot :: forall m a . MonadSpotify m => (Token -> ClientM a) -> m a
inSpot x = do
    tok <- getToken
    man <- getManager
    liftIO (runClientM (x tok) $ mkClientEnv man mainBase) >>= \case
        Left e -> expiry e >>= \case
            True -> retry
            False -> throwClientError e
        Right r -> return r
    where
        -- get a new token and try again
        --TODO log this?
        retry :: m a
        retry = do
            putToken =<< newToken
            inSpot x
        -- does the error indicate that the access token has expired?
        expiry :: ClientError -> m Bool
        expiry = \case
            FailureResponse _ resp ->
                if statusCode (responseStatusCode resp) == 401 then do
                    Error{message} <- liftEitherSpot $ bimap mkError error $ JSON.eitherDecode $ responseBody resp
                    if message == "The access token expired" then return True
                    else no
                else no
                where mkError s = DecodeFailure ("Failed to decode a spotify error: " <> T.pack s) resp
            _ -> no
            where no = return False

liftEitherSpot :: MonadSpotify m => Either ClientError a -> m a
liftEitherSpot = either throwClientError return

newToken :: MonadSpotify m => m Token
newToken = liftEitherSpot =<< liftIO =<< (newTokenIO <$> getAuth <*> getManager)

newTokenIO :: Auth -> Manager -> IO (Either ClientError Token)
newTokenIO a m = Token . accessToken <<$>> runClientM (requestToken a) (mkClientEnv m accountsBase)
  where
    requestToken (Auth (RefreshToken t) i s) = client (Proxy @AuthAPI)
        [ ("grant_type", "refresh_token"), ("refresh_token", t) ]
        (IdAndSecret i s)


{- Util -}

infixl 4 <<$>>
(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = fmap . fmap

(<<&>>) :: (Functor f, Functor g) => f (g a) -> (a -> b) -> f (g b)
(<<&>>) = flip (<<$>>)

uncurry3 :: (t1 -> t2 -> t3 -> t4) -> (t1, t2, t3) -> t4
uncurry3 f (x,y,z) = f x y z

tryError :: MonadError e m => m a -> m (Either e a)
tryError x = catchError (Right <$> x) $ return . Left

catchErrorBool :: MonadError e m => (e -> m Bool) -> m a -> (e -> m a) -> m a
catchErrorBool f x h = catchError x $ \e -> do
    b <- f e
    if b then h e else throwError e

--TODO wait for 'NoRecordSelectorFunctions' (and 'RecordDotSyntax') to avoid name clashes
identity :: a -> a
identity = Prelude.id
