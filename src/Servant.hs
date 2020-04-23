module Servant where --TODO explicit export list

-- we hide 'error' to ease deriving the FromJSON instance for Error' (who wants errors, anyway...)
import Prelude hiding (error)

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Aeson hiding (Error, (.:))
import Data.Bifunctor
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

import Data.Aeson.Types (Parser)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as SF
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as H
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import GHC.Generics (Generic)

import Types --TODO special naming for fields that clash with prelude? eg. 'id'


{- The Spotify monad -}

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

-- client authorization data
data Auth = Auth
    { refreshToken :: RefreshToken
    , clientId :: ClientId
    , clientSecret :: ClientSecret
    }

--TODO does 'runClientM' guarantee that no other types of exception are thrown?
runSpotify :: Maybe Manager -> Maybe Token -> Auth -> Spotify a -> IO (Either ClientError (a, Token))
runSpotify mm mt a x = do
    man <- maybe (newManager tlsManagerSettings) return mm
    let getTok = liftEither =<< liftIO (newTokenIO a man)
        rdr = runStateT (unSpot x) =<< maybe getTok return mt
    runExceptT $ runReaderT rdr (a, man)


{- Exposed types -}

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

--TODO do we want Strict?
type AuthHeader = Header' '[Strict,Required] "Authorization" Token

type AuthHeaderBasic = Header' '[Strict,Required] "Authorization" IdAndSecret

-- this aligns with the errors we get from Spotify via Servant
newtype Error' = Error' { error :: Error }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass FromJSON

{- Endpoints - see https://developer.spotify.com/documentation/web-api/reference-beta -}

nc :: NoContent -> ()
nc = const ()

c0 :: (MonadSpotify m, HasClient ClientM api,
         Client ClientM api ~ (Token -> ClientM a)) =>
        Proxy api -> m a
c0 = c0' identity identity
c1 :: (MonadSpotify m, HasClient ClientM api,
         Client ClientM api ~ (a1 -> Token -> ClientM a2)) =>
        Proxy api -> a1 -> m a2
c1 = c1' identity identity
c2 :: (MonadSpotify m, HasClient ClientM api,
         Client ClientM api ~ (a1 -> a2 -> Token -> ClientM a)) =>
        Proxy api -> a1 -> a2 -> m a
c2 = c2' identity identity
c3 :: (MonadSpotify m, HasClient ClientM api,
         Client ClientM api ~ (a1 -> a2 -> a4 -> Token -> ClientM a5)) =>
        Proxy api -> a1 -> a2 -> a4 -> m a5
c3 = c3' identity identity

j1 :: (JMap js, JT (MapSnd js) ~ (c, ())) => JRes js -> c
j1 = r1 . jRes

r1 :: (a, ()) -> a
r1 (a,()) = a

c0' :: (MonadSpotify f, HasClient ClientM api,
          Client ClientM api ~ (Token -> b1)) =>
         (b1 -> ClientM a) -> (a -> b2) -> Proxy api -> f b2
c0' f g p = (fmap g . inSpot) (f . client p)
c1' :: (MonadSpotify f, HasClient ClientM api,
          Client ClientM api ~ (a1 -> b1)) =>
         (b1 -> Token -> ClientM a2)
         -> (a2 -> b2) -> Proxy api -> a1 -> f b2
c1' f g p = (fmap g . inSpot) . (f . client p)
c2' :: (MonadSpotify f, HasClient ClientM api,
          Client ClientM api ~ (a1 -> b1)) =>
         (b1 -> a2 -> Token -> ClientM a3)
         -> (a3 -> b2) -> Proxy api -> a1 -> a2 -> f b2
c2' f g p = (fmap g . inSpot) .: (f . client p)
c3' :: (MonadSpotify f, HasClient ClientM api,
          Client ClientM api ~ (a1 -> b1)) =>
         (b1 -> a2 -> a3 -> Token -> ClientM a4)
         -> (a4 -> b2) -> Proxy api -> a1 -> a2 -> a3 -> f b2
c3' f g p = (fmap g . inSpot) .:. (f . client p)
c4' :: (MonadSpotify f, HasClient ClientM api,
          Client ClientM api ~ (a1 -> b1)) =>
         (b1 -> a2 -> a4 -> a5 -> Token -> ClientM a3)
         -> (a3 -> b2) -> Proxy api -> a1 -> a2 -> a4 -> a5 -> f b2
c4' f g p = (fmap g . inSpot) .:: (f . client p)
c5' :: (MonadSpotify f, HasClient ClientM api,
          Client ClientM api ~ (a1 -> b1)) =>
         (b1 -> a2 -> a4 -> a5 -> a3 -> Token -> ClientM a7)
         -> (a7 -> b2) -> Proxy api -> a1 -> a2 -> a4 -> a5 -> a3 -> f b2
c5' f g p = (fmap g . inSpot) .::. (f . client p)

type A (v :: StdMethod) a = AuthHeader :> Verb v 200 '[JSON] a
type B = ReqBody '[JSON]
type Q = QueryParam' '[Required]

--TODO separate into Map and Snd (or import library)
type family MapSnd (js :: [(k1,k2)]) :: [k2] where
    MapSnd '[] = '[]
    MapSnd ('(_,j) : js) = j : MapSnd js

type family JT (js :: [*]) where
    JT '[] = ()
    JT (j : js) = (j, JT js)

class JMap (js :: [(Symbol,*)]) where
    data JRes js
    jRes :: JRes js -> JT (MapSnd js)
    parseJSON' :: Object -> Parser (JRes js)
    type JFunc js r
    jFunc :: (JRes js -> r) -> JFunc js r
    toJSON' :: JRes js -> [(Text,Value)]
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
        Object o -> parseJSON' o
        v -> fail $ "JSON is not an object: " ++ show v

instance JMap js => ToJSON (JRes js) where
    toJSON = Object . H.fromList . toJSON'

--TODO we really ought to newtype this, seeing as comma separation isn't standard (also an orphan)
instance ToHttpApiData a => ToHttpApiData [a] where
    toQueryParam = T.intercalate "," . map toQueryParam



removeAlbums :: MonadSpotify f => [Text] -> f ()
removeAlbums = c1' identity nc $ Proxy @("me" :> "albums" :> Q "ids" [Text] :> A 'DELETE NoContent)

removeTracks :: MonadSpotify f => [Text] -> f ()
removeTracks = c1' identity nc $ Proxy @("me" :> "tracks" :> Q "ids" [Text] :> A 'DELETE NoContent)

--TODO fails silently - even through Spotify Web Console
saveTracks :: MonadSpotify m => [Text] -> m ()
saveTracks = c1' identity nc $ Proxy @("me" :> "tracks" :> Q "ids" [Text] :> A 'DELETE NoContent)

--TODO query params
getSavedTracks :: MonadSpotify m => m (Paging SavedTrack)
getSavedTracks = c0 $ Proxy @("me" :> "tracks" :> A 'GET (Paging SavedTrack))

--TODO we have to manually pass in our own username here, even though we seemingly never have permission to add a playlist for someone else
    -- store username in Reader env?
--TODO non-simple playlist doesn't seem to exist
createPlaylist :: MonadSpotify m => Text -> Text -> Bool -> Bool -> Text -> m PlaylistSimplified
createPlaylist = c5' jFunc identity $
    Proxy @("users" :> Capture "user_id" Text :> "playlists" :>
        B (JRes '[ '("name", Text), '("public", Bool), '("collaborative", Bool), '("description", Text) ]) :>
        A 'POST PlaylistSimplified)

--TODO int should be maybed
addToPlaylist :: MonadSpotify f => Text -> [Text] -> Int -> f Text
addToPlaylist = c3' jFunc j1 $ Proxy @("playlists" :> Capture "playlist_id" Text :> "tracks" :> B (JRes '[ '("uris", [Text]), '("position", Int) ]) :> A 'POST (JRes '[ '("snapshot_id", Text)]))

--TODO what does the label "id" actually do?
getAlbum :: MonadSpotify m => Text -> m Album
getAlbum = c1 $ Proxy @("albums" :> Capture "id" Text :> A 'GET Album)

getAlbumTracks :: MonadSpotify m => Text -> m (Paging TrackSimplified)
getAlbumTracks = c1 $ Proxy @("albums" :> Capture "id" Text :> "tracks" :> A 'GET (Paging TrackSimplified))

getUser :: MonadSpotify m => Text -> m UserPublic
getUser = c1 $ Proxy @("users" :> Capture "user_id" Text :> A 'GET UserPublic)

getMe :: MonadSpotify m => m UserPublic
getMe = c0 $ Proxy @("me" :> A 'GET UserPublic)

getTrack :: MonadSpotify m => Text -> m Track
getTrack = c1 $ Proxy @("tracks" :> Capture "id" Text :> A 'GET Track)

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
                    Error{message} <- liftEitherSpot $ bimap mkError error $ eitherDecode $ responseBody resp
                    if message == "The access token expired" then return True
                    else no
                else no
                where mkError s = DecodeFailure ("Failed to decode a spotify error: " <> T.pack s) resp
            _ -> no
            where no = return False

liftEitherSpot :: MonadSpotify m => Either ClientError a -> m a
liftEitherSpot = either throwClientError return

requestToken :: Auth -> ClientM TokenResponse
requestToken (Auth (RefreshToken t) i s) = client (Proxy @AuthAPI)
    [ ("grant_type", "refresh_token"), ("refresh_token", t) ]
    (IdAndSecret i s)

newTokenIO :: Auth -> Manager -> IO (Either ClientError Token)
newTokenIO a m = Token . accessToken <<$>> runClientM (requestToken a) (mkClientEnv m accountsBase)

newToken :: MonadSpotify m => m Token
newToken = liftEitherSpot =<< liftIO =<< (newTokenIO <$> getAuth <*> getManager)


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

(.:) :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
(.:) = (.) . (.)

(.:.) :: (b -> c) -> (a1 -> a2 -> a -> b) -> a1 -> a2 -> a -> c
(.:.) = (.:) . (.)

(.::) :: (b -> c)
           -> (a1 -> a2 -> a4 -> a5 -> b) -> a1 -> a2 -> a4 -> a5 -> c
(.::) = (.:.) . (.)

(.::.) :: (b -> c)
            -> (a1 -> a2 -> a4 -> a5 -> a -> b)
            -> a1
            -> a2
            -> a4
            -> a5
            -> a
            -> c
(.::.) = (.::) . (.)

--TODO wait for 'NoRecordSelectorFunctions' (and 'RecordDotSyntax') to avoid name clashes
identity :: a -> a
identity = Prelude.id
