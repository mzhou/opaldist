import System.Environment (getArgs)

import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Query.SP (spLength)

import Opal.Distance.Stations (Station)
import Opal.Distance.Tracks (graph)

main = do
    (orig:dest:_) <- getArgs
    putStrLn $ show $ spLength (fromEnum $ ((read orig) :: Station)) (fromEnum $ ((read dest) :: Station)) (graph :: Gr Station Int)
