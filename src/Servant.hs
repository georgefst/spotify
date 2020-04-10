module Servant where --TODO explicit export list

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Aeson
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

runSpotify :: Maybe Manager -> Maybe Token -> Auth -> Spotify a -> IO (Either ClientError a)
runSpotify mm mt a x = do
    man <- maybe (newManager tlsManagerSettings) return mm
    let env = (a, mkClientEnv man accountsBase)
        rdr = evalStateT (unSpot x) =<< maybe getTokenSpot return mt
        cli = runExceptT $ runReaderT rdr env
    join <$> runClientM cli (mkClientEnv man mainBase)

-- simple way to run a single action - for full power use the Spotify monad
runAction :: Auth -> Action a -> IO (Either ClientError a)
runAction a = runSpotify Nothing Nothing a . inSpot


{- Exposed types -}

newtype RefreshToken = RefreshToken Text
    deriving (Eq,Ord,Show)
newtype ClientId = ClientId ByteString
    deriving (Eq,Ord,Show)
newtype ClientSecret = ClientSecret ByteString
    deriving (Eq,Ord,Show)


{- Internal helper types -}

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
quickRun :: Show a => Action a -> IO ()
quickRun x = do
    i <- myClientId
    s <- myClientSecret
    rt <- myRefreshToken
    pPrint =<< runAction (Auth rt i s) x
reallyQuickRun :: Show a => Action a -> IO () -- unsafe once token expires
reallyQuickRun x = pPrint =<< runSpotify Nothing (Just myToken) (error "no auth data") (inSpot x)
quickTok :: IO ()
quickTok = do
    man <- newManager tlsManagerSettings
    let env = mkClientEnv man accountsBase
    i <- myClientId
    s <- myClientSecret
    rt <- myRefreshToken
    x <- getToken' (Auth rt i s, env)
    pPrint x --TODO pPrint only prints half the error compared to 'print' - must be a bug in pretty-simple - investigate
myToken :: Token
myToken = Token
    "BQAxO3XNOZjPtNxcHLlIeEveEz7Pd1qenF2lF8pFoPPG_eT-7sEqsXe5QEilpPiPmsz2lXu3u8orV2wWgCRvfTq_0kbcxb7P8TDSMyGZlxHdg1PxvEV5x0NVOpYUifcBtE6o5pES54tjF93z3JRpcVRVhDEBIPVVtlH3wqjqE3M0ZqaG5CToWTK_ZwvNoN5f1ISJWHq5jZ164GkmbtBbS0tZPXHx02A3-QMgs2_tzPJBDkky9ayRNW1nFKCjq3x-I1MCfytsmY7Ve1qeOSb5BVo"


{- Util -}

infixl 4 <<$>>
(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = fmap . fmap

(<<&>>) :: (Functor f, Functor g) => f (g a) -> (a -> b) -> f (g b)
(<<&>>) = flip (<<$>>)

uncurry3 :: (t1 -> t2 -> t3 -> t4) -> (t1, t2, t3) -> t4
uncurry3 f (x,y,z) = f x y z
