{-# LANGUAGE OverloadedStrings #-}
module Web.DummyMe.Response (
      renderStatus
    ) where

import Data.Aeson
import Data.Scientific
import Data.Text.Encoding
import Network.HTTP.Types.Status

renderStatus :: Status -> Value
renderStatus status = object [
      "status"  .= Number code
    , "message" .= String msg
    ]
    where
       code = scientific (fromIntegral $ statusCode status) 0
       msg  = decodeUtf8 $ statusMessage status
