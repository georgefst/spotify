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
import Data.Generics.Labels
import Data.Maybe
import Data.Monoid
import Data.Proxy
import Lens.Micro.Extras
import Network.HTTP.Client (Manager,newManager)
import Network.HTTP.Client.TLS
import Network.HTTP.Types (Status(statusCode))
import Servant.API
import Servant.Client
import System.Directory
import System.FilePath
import Text.Pretty.Simple

import Data.Aeson (FromJSON,eitherDecode)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as SF
import qualified Data.ByteString.Lazy as BL
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
            a <- getAuth
            m <- getManager
            liftEitherSpot =<< newToken a m
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

-- covers the common use case
-- use 'runSpotify'' for more control
-- can throw 'ClientError'
runSpotify :: Auth -> Spotify a -> IO a
runSpotify a x = either throwIO (return . fst) =<< runSpotify' Nothing Nothing a x

--TODO does 'runClientM' guarantee that no other types of exception are thrown?
runSpotify' :: Maybe Manager -> Maybe Token -> Auth -> Spotify a -> IO (Either ClientError (a, Token))
runSpotify' mm mt a x = do
    man <- maybe (newManager tlsManagerSettings) return mm
    let getTok = liftEither =<< liftIO (newToken a man)
        rdr = runStateT (unSpot x) =<< maybe getTok return mt
    runExceptT $ runReaderT rdr (a, man)

-- simple way to run a single action - for full power use the Spotify monad
-- can throw 'ClientError'
-- note that this sets up a new TLS connection each time, and contacts spotify servers to get a temporary access token
    -- these often take longer than the actual action
--TODO consider making this obolete by exporting 'Spotify a's rather than 'Action a's
    -- thus not exporting 'Action' or 'inSpot'
runAction :: Auth -> Action a -> IO a
runAction a = runSpotify a . inSpot


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


{- Endpoints -}

--TODO export opaquely
newtype Action a = Action (Token -> ClientM a)

--TODO what does the label "user_id" actually do?
type UserAPI = "users" :> Capture "user_id" Text :> AuthHeader :> Get '[JSON] UserPublic
getUser :: Text -> Action UserPublic
getUser = Action . client (Proxy @UserAPI)

type MeAPI = "me" :> AuthHeader :> Get '[JSON] UserPublic
getMe :: Action UserPublic
getMe = Action $ client (Proxy @MeAPI)

type TrackAPI = "tracks" :> Capture "id" Text :> AuthHeader :> Get '[JSON] Track
getTrack :: Text -> Action Track
getTrack = Action . client (Proxy @TrackAPI)

type AuthAPI = "token" :> ReqBody '[FormUrlEncoded] [(Text, Text)] :> AuthHeaderBasic :> Post '[JSON] TokenResponse


{- Constants -}

mainBase, accountsBase :: BaseUrl
mainBase = BaseUrl Http "api.spotify.com" 80 "v1"
accountsBase = BaseUrl Http "accounts.spotify.com" 80 "api"


{- Helpers -}

inSpot :: forall m a. MonadSpotify m => Action a -> m a
inSpot a@(Action x) = do
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
            putToken =<< getTokenSpot
            inSpot a -- try again with the new token
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

newToken :: Auth -> Manager -> IO (Either ClientError Token)
newToken a m = Token . accessToken <<$>> runClientM (requestToken a) (mkClientEnv m accountsBase)

getTokenSpot :: MonadSpotify m => m Token
getTokenSpot = do
    a <- getAuth
    m <- getManager
    liftEitherSpot =<< liftIO (newToken a m)


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
