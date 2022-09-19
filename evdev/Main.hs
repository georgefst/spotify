{- HLINT ignore "Redundant <$>" -}

import Control.Monad.IO.Class
import Data.Text qualified as T
import Data.Text.Encoding
import Evdev
import Evdev.Codes (Key (KeyNextsong, KeyPlaypause, KeyPrevioussong))
import Evdev.Stream
import Spotify
import Spotify.Servant.Player
import Spotify.Types.Auth
import Spotify.Types.Player
import Streamly.Internal.Data.Stream.IsStream (hoist)
import Streamly.Prelude qualified as S
import System.Environment
import System.Exit
import Text.Pretty.Simple

main :: IO ()
main =
    (map T.pack <$> getArgs) >>= \case
        [devPath, ClientId -> clientId, ClientSecret -> clientSecret, RefreshToken -> refreshToken] -> do
            dev <- newDevice (encodeUtf8 devPath)
            (pPrint =<<) $ runSpotify (Auth{..}) $ flip S.mapM_ (hoist liftIO $ readEvents dev) \case
                Event{eventData} | KeyEvent k Pressed <- eventData -> case k of
                    KeyPlaypause -> do
                        state <- getPlaybackState Nothing
                        if state.isPlaying
                            then pausePlayback Nothing
                            else startPlayback Nothing emptyStartPlaybackOpts
                    KeyNextsong -> skipToNext Nothing
                    KeyPrevioussong -> skipToPrevious Nothing
                    _ -> pure ()
                _ -> pure ()
        _ -> putStrLn "bad args" >> exitFailure
