module Main where

import Foreign.C.String
import Data.ByteString as BS (useAsCString)
import GHC.JS.Prim
import GHC.JS.Foreign.Callback
import Lib

foreign import javascript "((arr,offset) => document.getElementById(\"output\").innerHTML = h$decodeUtf8z(arr,offset))"
  setInnerHtml :: CString -> IO ()

foreign import javascript "((f) => { h$getJSON = f })"
  setCallback :: Callback (JSVal -> IO ()) -> IO ()

main :: IO ()
main = asyncCallback1 (flip BS.useAsCString setInnerHtml . getJson . fromJSString) >>= setCallback
