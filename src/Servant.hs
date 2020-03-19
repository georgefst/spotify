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

--TODO use a record
type SpotifyEnv = (RefreshToken, ClientId, ClientSecret, ClientEnv)

runSpot :: Maybe Manager -> RefreshToken -> ClientId -> ClientSecret -> Maybe Token -> SpotifyAction a -> IO (Either ClientError a)
runSpot mm r i s mt x = do
    man <- maybe (newManager tlsManagerSettings) return mm
    let env = (r, i, s, mkClientEnv man accountsBase)
        rdr = evalStateT (unSpot $ inSpot x) =<< maybe getTokenSpot return mt
        cli = runExceptT $ runReaderT rdr env
    join <$> runClientM cli (mkClientEnv man mainBase)


{- Exposed types -}

newtype RefreshToken = RefreshToken Text
newtype ClientId = ClientId ByteString
newtype ClientSecret = ClientSecret ByteString


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

newtype SpotifyAction a = SpotifyAction (Token -> ClientM a)

--TODO what does the label "user_id" actually do?
type UserAPI = "users" :> Capture "user_id" Text :> AuthHeader :> Get '[JSON] UserPublic
getUser :: Text -> SpotifyAction UserPublic
getUser = SpotifyAction . client (Proxy @UserAPI)

type MeAPI = "me" :> AuthHeader :> Get '[JSON] UserPublic
getMe :: SpotifyAction UserPublic
getMe = SpotifyAction $ client (Proxy @MeAPI)

type TrackAPI = "tracks" :> Capture "id" Text :> AuthHeader :> Get '[JSON] Track
getTrack :: Text -> SpotifyAction Track
getTrack = SpotifyAction . client (Proxy @TrackAPI)

type AuthAPI = "token" :> ReqBody '[FormUrlEncoded] [(Text, Text)] :> AuthHeaderBasic :> Post '[JSON] TokenResponse


{- Constants -}

mainBase, accountsBase :: BaseUrl
mainBase = BaseUrl Http "api.spotify.com" 80 "v1"
accountsBase = BaseUrl Http "accounts.spotify.com" 80 "api"


{- Helpers -}

inSpot :: SpotifyAction a -> Spotify a
inSpot (SpotifyAction x) = liftSpot . x =<< get

liftSpot :: ClientM a -> Spotify a
liftSpot = Spotify . lift . lift . lift

getToken :: RefreshToken -> ClientId -> ClientSecret -> ClientM TokenResponse
getToken (RefreshToken t) i s = client (Proxy @AuthAPI)
    [ ("grant_type", "refresh_token"), ("refresh_token", t) ]
    (IdAndSecret i s)

getToken' :: SpotifyEnv -> IO (Either ClientError Token)
getToken' (t, i, s, e) = Token . view #accessToken <<$>> runClientM (getToken t i s) e

getTokenSpot :: (MonadIO m, MonadReader SpotifyEnv m, MonadError ClientError m) => m Token
getTokenSpot = join . liftIO . fmap liftEither . getToken' =<< ask


--TODO remove before release
{- Personal helpers -}

myRefreshToken :: IO RefreshToken
myRefreshToken = RefreshToken <$> T.readFile "/home/george/stuff/spotify-data/refresh"
myClientId :: IO ClientId
myClientId = ClientId <$> BS.readFile "/home/george/stuff/spotify-data/id"
myClientSecret :: IO ClientSecret
myClientSecret = ClientSecret <$> BS.readFile "/home/george/stuff/spotify-data/secret"
quickRun :: Show a => SpotifyAction a -> IO ()
quickRun x = do
    i <- myClientId
    s <- myClientSecret
    rt <- myRefreshToken
    pPrint =<< runSpot Nothing rt i s Nothing x
reallyQuickRun :: Show a => SpotifyAction a -> IO () -- unsafe once token expires
reallyQuickRun x = pPrint =<< runSpot Nothing undefined undefined undefined (Just myToken) x
quickTok :: IO ()
quickTok = do
    man <- newManager tlsManagerSettings
    let env = mkClientEnv man accountsBase
    i <- myClientId
    s <- myClientSecret
    rt <- myRefreshToken
    x <- getToken' (rt,i,s,env)
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
