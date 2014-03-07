module Opal.Distance.Calc where

import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Query.SP (spLength)

import Opal.Distance.Stations (Station)
import Opal.Distance.Tracks (Metres, graph)

trackDistance :: Station -> Station -> Metres
trackDistance orig dest = spLength (fromEnum orig) (fromEnum dest) (graph :: Gr Station Int)
