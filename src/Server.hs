{-# LANGUAGE OverloadedStrings #-}

import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Query.SP (spLength)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Text (unpack)
import Network.HTTP.Types (status200)
import Network.Wai (pathInfo, responseLBS)
import Network.Wai.Handler.Warp (run)

import Opal.Distance.Stations (Station)
import Opal.Distance.Tracks (graph)

application request = let
    (_:orig:dest:_) = map unpack $ pathInfo request
    in
    return $ responseLBS status200 [("Content-Type", "text/plain;charset=utf-8")] $ pack $
        show $ spLength (fromEnum $ ((read $ orig) :: Station)) (fromEnum $ ((read $ dest) :: Station)) (graph :: Gr Station Int)

main = run 6725 application
