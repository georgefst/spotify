{- HLINT ignore "Use fmap" -}

module Main (main) where

import Control.Monad
import Data.Foldable (for_)
import Data.Function
import Data.Functor
import Data.Proxy
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Lucid (Html, ToHtml (toHtml), div_, h1_, href_, link_, onclick_, rel_)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.Wai.Application.Static (defaultWebAppSettings, staticApp)
import Network.Wai.Handler.Warp (Port, defaultSettings, runSettings, setBeforeMainLoop, setPort)
import Options.Applicative
import Servant (Get, QueryParam', Raw, Required, Strict, linkURI, safeLink, serve, type (:<|>) ((:<|>)), type (:>))
import Servant.Client (showBaseUrl)
import Servant.HTML.Lucid (HTML)
import Spotify
import Spotify.Servant (AccountsAPI)
import Web.Browser (openBrowser)

main :: IO ()
main = join $ execParser $ flip info mempty $ run <$> option auto (long "port") <*> strOption (long "client-id") <*> strOption (long "client-secret")

type API = (QueryParam' '[Required, Strict] "code" AuthCode :> Get '[HTML] (Html ())) :<|> Raw

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
        . serve (Proxy @API)
        $ ( \authCode ->
                newTokenIO' man clientId clientSecret redirect authCode <&> either (toHtml . show) \resp -> do
                    link_ [rel_ "stylesheet", href_ "login/style.css"]
                    for_
                        [("Access token:", resp.accessToken.unwrap), ("Refresh token:", resp.refreshToken.unwrap)]
                        \(t, c) -> div_ [] do
                            h1_ [onclick_ $ "navigator.clipboard.writeText('" <> c <> "')"] t
                            toHtml c
          )
            :<|> pure (staticApp $ defaultWebAppSettings ".")
  where
    redirect = URL $ "http://127.0.0.1:" <> T.pack (show port)
