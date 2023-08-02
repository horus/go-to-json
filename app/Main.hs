{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Main where

import Lib
import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Handler.Warp

main :: IO ()
main = do
  run 1983 $ \req respond -> do
    bs <- getRequestBodyChunk req
    respond $ responseLBS status200 [("Content-Type", "text/plain")] (getJson bs)
