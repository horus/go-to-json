{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Network.HTTP.Types.Status
import Network.Wai
import Network.Wai.Handler.Warp

main :: IO ()
main = run 1983 $ \req respond -> do
   bs <- getRequestBodyChunk req
   respond $ responseLBS status200 [] (getJson bs)
