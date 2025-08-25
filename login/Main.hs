{- HLINT ignore "Use fmap" -}

module Main (main) where

import Control.Monad
import Data.Function
import Data.Functor
import Data.Proxy
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Lucid (Html, ToHtml (toHtml), div_, pre_)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.Wai.Handler.Warp (Port, defaultSettings, runSettings, setBeforeMainLoop, setPort)
import Options.Applicative
import Servant (Get, QueryParam', Required, Strict, linkURI, safeLink, serve, type (:>))
import Servant.Client (showBaseUrl)
import Servant.HTML.Lucid (HTML)
import Spotify
import Spotify.Servant (AccountsAPI)
import Web.Browser (openBrowser)

main :: IO ()
main = join $ execParser $ flip info mempty $ run <$> option auto (long "port") <*> strOption (long "client-id") <*> strOption (long "client-secret")

type API = QueryParam' '[Required, Strict] "code" AuthCode :> Get '[HTML] (Html ())

run :: Port -> ClientId -> ClientSecret -> IO ()
run port clientId clientSecret = do
    T.putStrLn $
        "Make sure you have "
            <> redirect.unwrap
            <> " configured as a Redirect URI in your Spotify developer dashboard!"
    man <- newTlsManager
    runSettings
        ( defaultSettings
            & setBeforeMainLoop do
                let mkLink = safeLink (Proxy @AccountsAPI) (Proxy @Authorize)
                    link = mkLink clientId "code" redirect Nothing (Just $ ScopeSet allScopes) Nothing
                success <- openBrowser $ showBaseUrl accountsBase <> "/" <> show (linkURI link)
                when (not success) $ T.putStrLn "Failed to open browser"
            & setPort port
        )
        $ serve (Proxy @API) \authCode ->
            newTokenIO' man clientId clientSecret redirect authCode <&> either (toHtml . show) \resp -> do
                div_ "Access token:"
                pre_ $ toHtml resp.accessToken.unwrap
                div_ "Refresh token:"
                pre_ $ toHtml resp.refreshToken.unwrap
  where
    redirect = URL $ "http://127.0.0.1:" <> T.pack (show port)
