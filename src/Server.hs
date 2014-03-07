{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString.Lazy.Char8 (pack)
import Data.Text (unpack)
import Network.HTTP.Types (status200)
import Network.Wai (pathInfo, responseLBS)
import Network.Wai.Handler.Warp (run)
import System.Environment (getArgs)

import Opal.Distance.Stations (Station)
import Opal.Distance.Calc (fareDistance)

application request = let
    (_:orig:dest:_) = map unpack $ pathInfo request
    in
    return $ responseLBS status200 [("Content-Type", "text/plain;charset=utf-8"), ("Access-Control-Allow-Origin", "*")] $ pack $
        show $ fareDistance ((read orig) :: Station) ((read dest) :: Station)

main = do
    [port] <- getArgs
    run (read port) application
