{-# OPTIONS_GHC -Wno-orphans #-}

module Orphans.Servant.Lucid where

import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Servant.API (MimeUnrender (mimeUnrender))
import Servant.HTML.Lucid (HTML)

-- https://github.com/haskell-servant/servant-lucid/issues/14#issuecomment-1250060470
instance MimeUnrender HTML Text where
    mimeUnrender _ = pure . decodeUtf8 . BL.toStrict
