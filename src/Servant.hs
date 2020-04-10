module Servant where --TODO explicit export list

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Generics.Labels
import Data.Maybe
import Data.Monoid
import Data.Proxy
import Lens.Micro.Extras
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Client.TLS
import Servant.API
import Servant.Client
import Text.Pretty.Simple

import Data.Aeson (decode)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as SF
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
--TODO separate ClientError from token refresh from the one from the endpoint we care about?
--TODO should we provide a transformer? ClientM doesn't...
--TODO should I call this SpotifyM?
--TODO catch any error caused by token timeout
newtype Spotify a = Spotify {
    unSpot :: StateT Token (ReaderT SpotifyEnv (ExceptT ClientError ClientM)) a}
    deriving newtype (Functor, Applicative, Monad, MonadIO,
        MonadState Token, MonadReader SpotifyEnv, MonadError ClientError)

--TODO use a record?
type SpotifyEnv = (Auth, ClientEnv)

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
    let env = (a, mkClientEnv man accountsBase)
        rdr = runStateT (unSpot x) =<< maybe getTokenSpot return mt
        cli = runExceptT $ runReaderT rdr env
    join <$> runClientM cli (mkClientEnv man mainBase)

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
    toUrlPiece (IdAndSecret (ClientId i) (ClientSecret s)) = toUrlPiece . ("Basic " <>) . T.decodeUtf8 . SF.encode $ i <> ":" <> s

--TODO do we want Strict?
type AuthHeader = Header' '[Strict,Required] "Authorization" Token

type AuthHeaderBasic = Header' '[Strict,Required] "Authorization" IdAndSecret


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

inSpot :: Action a -> Spotify a
inSpot (Action x) = liftSpot . x =<< get

liftSpot :: ClientM a -> Spotify a
liftSpot = Spotify . lift . lift . lift

getToken :: Auth -> ClientM TokenResponse
getToken (Auth (RefreshToken t) i s) = client (Proxy @AuthAPI)
    [ ("grant_type", "refresh_token"), ("refresh_token", t) ]
    (IdAndSecret i s)

getToken' :: SpotifyEnv -> IO (Either ClientError Token)
getToken' (a, e) = Token . accessToken <<$>> runClientM (getToken a) e

getTokenSpot :: (MonadIO m, MonadReader SpotifyEnv m, MonadError ClientError m) => m Token
getTokenSpot = join . liftIO . fmap liftEither . getToken' =<< ask


--TODO remove before release
{- Personal helpers -}

myRefreshToken :: IO RefreshToken
myRefreshToken = RefreshToken <$> T.readFile "/home/gthomas/personal/spotify-data/refresh"
myClientId :: IO ClientId
myClientId = ClientId <$> BS.readFile "/home/gthomas/personal/spotify-data/id"
myClientSecret :: IO ClientSecret
myClientSecret = ClientSecret <$> BS.readFile "/home/gthomas/personal/spotify-data/secret"
myAuth :: IO Auth
myAuth = Auth <$> myRefreshToken <*> myClientId <*> myClientSecret
myAccessToken :: IO Token
myAccessToken = Token <$> T.readFile "/home/gthomas/personal/spotify-data/token"
quickRun :: Show a => Action a -> IO ()
quickRun x = do
    a <- myAuth
    pPrint =<< runAction a x --TODO pPrint only prints half the error compared to 'print' - must be a bug in pretty-simple - investigate
reallyQuickRun :: Action a -> IO a -- unsafe once token expires
reallyQuickRun x = do
    t <- myAccessToken
    either throwIO (return . fst) =<< runSpotify' Nothing (Just t) (error "no auth data") (inSpot x)
newTok :: IO (Either ClientError Token)
newTok = do
    man <- newManager tlsManagerSettings
    let env = mkClientEnv man accountsBase
    a <- myAuth
    t <- getToken' (a, env)
    case t of
        Left _ -> putStrLn "failure"
        Right (Token tt) -> T.writeFile "/home/gthomas/personal/spotify-data/token" tt
    return t


{- Util -}

infixl 4 <<$>>
(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = fmap . fmap

(<<&>>) :: (Functor f, Functor g) => f (g a) -> (a -> b) -> f (g b)
(<<&>>) = flip (<<$>>)

uncurry3 :: (t1 -> t2 -> t3 -> t4) -> (t1, t2, t3) -> t4
uncurry3 f (x,y,z) = f x y z
