module Opal.Distance.Tracks where

import Data.Graph.Inductive.Graph

import Opal.Distance.Stations

type Metres = Int
type Dist = (Station, Metres)
type Track = LEdge Metres

graph :: Graph gr => gr Station Metres
graph = let
    in
    mkGraph nodeList edgeList

nodeList = [(fromEnum x, x) | x <- stations]

edgeList = let
    edgeList'
        =  cityEdgeList
        ++ casulaCentralEdgeList
        ++ bondiKingsCrossEdgeList
        ++ hornsbyWynyardEdgeList
        ++ eppingWynyardEdgeList
        ++ cabraCentralEdgeList
        ++ clydeCarlingfordEdgeList
        ++ lidcombeLithgowEdgeList
        ++ blacktownRichmondEdgeList
        ++ strathfieldDungogEdgeList
        ++ hornsbyCentralEdgeList
        ++ flemingtonOlympicParkEdgeList
    in
    edgeList' ++ [(y, x, z) | (x, y, z) <- edgeList']

-- Page 72
cityStations :: [Station]
cityStations = [
    Central,
    TownHall,
    Wynyard,
    CircularQuay,
    MartinPlace,
    KingsCross,
    StJames,
    Museum]

cityEdgeList :: [Track]
cityEdgeList = [(x, y, 3210) | (x, y) <- handshakes $ map fromEnum cityStations]

-- Page 73
casulaCentralDists :: [Dist]
casulaCentralDists = [
    (Central, 0),
    (Redfern, 1310),
    (Macdonaldtown, 2500),
    (Newtown, 3120),
    (Stanmore, 4670),
    (Petersham, 5490),
    (Lewisham, 6260),
    (SummerHill, 7050),
    (Ashfield, 8400),
    (Croydon, 9440),
    (Burwood, 10620),
    (Strathfield, 11730),
    (Homebush, 12740),
    (Flemington, 14320),
    (Lidcombe, 16580),
{-
    (Berala, 18350),
    (RegentsPark, 19880),
    (Sefton, 21180),
    (ChesterHill, 22310),
    (Leightonfield, 23660),
    (Villawood, 24480),
    (Carramar, 25900),
-}
    (Cabramatta, 28450),
    (WarwickFarm, 30540),
    (Liverpool, 32090),
    (Casula, 35270)]

casulaCentralEdgeList :: [Track]
casulaCentralEdgeList = chainEdgeList casulaCentralDists

-- Page 74, via RegentsPark
{-
glenfieldCentralIwDists :: [Dist]
glenfieldCentralIwDists = [
    (Central, 0),
    (Glenfield, 38320),
    (MacquarieFields, 40270),
    (Ingleburn, 42100),
    (Minto, 46150),
    (Leumeah, 49020),
    (Campbelltown, 51100)
    (MenanglePark, 59280),
    (Menangle, 61760),
    (DouglasPark, 69750),
    (Picton, 81670),
    (Tahmoor, 91000),
    (Bargo, 100000),
    (Yerrinbool, 113000),
    (Mittagong, 129000),
    (Bowral, 133000),
    (Burradoo, 136000),
    (MossVale, 143000),
    (Exeter, 153000),
    (Bundanoon, 159000),
    (Penrose, 168000),
    (Wingello, 174000),
    (Tallong, 182000),
    (Murulan, 190000),
    (Goulburn, 222000)]
-}

-- Page 74, via East Hills
-- TODO

-- Page 75
bondiKingsCrossDists :: [Dist]
bondiKingsCrossDists = [
    (KingsCross, 0),
    (Edgecliff, 1420),
    (BondiJunction, 3400)]

bondiKingsCrossEdgeList :: [Track]
bondiKingsCrossEdgeList = chainEdgeList bondiKingsCrossDists

-- Page 75
hornsbyWynyardDists :: [Dist]
hornsbyWynyardDists = [
    (Wynyard, 0),
    (MilsonsPoint, 2390),
    (NorthSydney, 3100),
    (Waverton, 4100),
    (Wollstonecraft, 5130),
    (StLeonards, 6380),
    (Artarmon, 8290),
    (Chatswood, 9640),
    (Roseville, 11270),
    (Lindfield, 12570),
    (Killara, 13860),
    (Gordon, 15090),
    (Pymble, 16860),
    (Turramurra, 18790),
    (Warrawee, 19860),
    (Wahroonga, 20740),
    (Waitara, 22170),
    (Hornsby, 23240)]

hornsbyWynyardEdgeList :: [Track]
hornsbyWynyardEdgeList = chainEdgeList hornsbyWynyardDists

-- Page 75
eppingWynyardDists :: [Dist]
eppingWynyardDists = [
    -- (Wynyard, 0), -- Duplicate
    (Chatswood, 9640),
    (NorthRyde, 16180),
    (MacquariePark, 17590),
    (MacquarieUniversity, 18860),
    (Epping, 22780)]

eppingWynyardEdgeList :: [Track]
eppingWynyardEdgeList = chainEdgeList eppingWynyardDists

-- Page 76
cabraCentralDists :: [Dist]
cabraCentralDists = [
    (Central, 0),
    (Granville, 21380),
    (Merrylands, 23500),
    (Guildford, 25710),
    (Yennora, 27440),
    (Fairfield, 28970),
    (CanleyVale, 31000),
    (Cabramatta, 32030)]

cabraCentralEdgeList :: [Track]
cabraCentralEdgeList = chainEdgeList cabraCentralDists

-- Page 76
-- TODO: Illawarra

-- Page 77
-- TODO: Sydneham - Lidcombe

-- Page 77
-- TODO: Tempe - Glenfield - Macarthur

-- Page 78
-- TODO: Sutherland - Cronulla

-- Page 78
clydeCarlingfordDists :: [Dist]
clydeCarlingfordDists = [
    (Central, 0),
    (Clyde, 20640),
    (Rosehill, 22370),
    (Rydalmere, 24020),
    (Dundas, 24820),
    (Telopea, 26330),
    (Carlingford, 27860)]

clydeCarlingfordEdgeList :: [Track]
clydeCarlingfordEdgeList = chainEdgeList clydeCarlingfordDists

-- Page 78
-- TODO: Coniston - Port Kembla

-- Page 79
-- TODO: Unanderra - Moss Vale

-- Page 79
lidcombeLithgowDists :: [Dist]
lidcombeLithgowDists = [
    (Central, 0),
    (Lidcombe, 16580),
    (Auburn, 18650),
    -- (Clyburn, 19860), -- Rubble
    (Clyde, 20640),
    (Granville, 21380),
    (HarrisPark, 22540),
    (Parramatta, 23200),
    (Westmead, 25110),
    (Wentworthville, 26720),
    (PendleHill, 28260),
    (Toongabbie, 29930),
    (SevenHills, 32090),
    (Blacktown, 34800),
    (Doonside, 38550),
    (RootyHill, 40940),
    (MountDruitt, 43900),
    (StMarys, 47480),
    (Werrington, 49150),
    (Kingswood, 52670),
    (Penrith, 55100),
    (EmuPlains, 57470)]
-- TODO: Lapstone - Lithgow

lidcombeLithgowEdgeList :: [Track]
lidcombeLithgowEdgeList = chainEdgeList lidcombeLithgowDists

-- Page 80
blacktownRichmondDists :: [Dist]
blacktownRichmondDists = [
    (Central, 0),
    (Blacktown, 34800),
    (Marayong, 37380),
    (QuakersHill, 40090),
    (Schofields, 43400),
    (Riverstone, 45950),
    (Vineyard, 49330),
    (Mulgrave, 52590),
    (Windsor, 54980),
    (Clarendon, 57270),
    (EastRichmond, 59060),
    (Richmond, 60670)]

blacktownRichmondEdgeList :: [Track]
blacktownRichmondEdgeList = chainEdgeList blacktownRichmondDists

-- Page 80
strathfieldDungogDists :: [Dist]
strathfieldDungogDists = [
    (Central, 0),
    (Strathfield, 11730),
    (NorthStrathfield, 13400),
    (ConcordWest, 14550),
    (Rhodes, 16560),
    (Meadowbank, 18190),
    (WestRyde, 19190),
    (Denistone, 20160),
    (Eastwood, 21420),
    (Epping, 23320),
    (Cheltenham, 25390),
    (Beecroft, 26900),
    (PennantHills, 28570),
    (Thornleigh, 29410),
    (Normanhurst, 31740),
    (Hornsby, 33860),
    (Asquith, 35710),
    (MountColah, 37680),
    (MountKuringGai, 40680),
    (Berowra, 44660),
    (Cowan, 48800),
    (HawkesburyRiver, 57370),
    (Wondobyne, 65160),
    (WoyWoy, 72600),
    (Koolewong, 74830),
    (Tascott, 76910),
    (PointClare, 78050),
    (Gosford, 80910),
    (Narara, 84590),
    (NiagaraPark, 86200),
    (Lisarow, 87730),
    (Ourimbah, 90610),
    (Tuggerah, 98530),
    (Wyong, 101050)]
    -- TODO: Warnervale - Dungog

strathfieldDungogEdgeList :: [Track]
strathfieldDungogEdgeList = chainEdgeList strathfieldDungogDists

-- Page 82
hornsbyCentralDists :: [Dist]
hornsbyCentralDists = [
    (Central, 0),
    (Wahroonga, 36360),
    (Warrawee, 37240),
    (Turramurra, 38310),
    (Pymble, 40240),
    (Gordon, 42010),
    (Killara, 43240),
    (Lindfield, 44530),
    (Roseville, 45830),
    (Chatswood, 47460),
    (Artarmon, 48810),
    (StLeonards, 50720),
    (Wollstonecraft, 51970),
    (Waverton, 53000),
    (NorthSydney, 54000),
    (MilsonsPoint, 54710),
    (Wynyard, 57100)]

hornsbyCentralEdgeList :: [Track]
hornsbyCentralEdgeList = chainEdgeList hornsbyCentralDists

-- Page 83
-- TODO: Maitland - Scone

-- Page 83
-- TODO: Central - Wolli Creek

-- Page 83
flemingtonOlympicParkDists :: [Dist]
flemingtonOlympicParkDists = [
    (OlympicPark, 0),
    (Flemington, 2400)]
-- NOTE: Lidcome to Olympic Park is Lidcome to Flemington + Flemington to Olympic Park

flemingtonOlympicParkEdgeList :: [Track]
flemingtonOlympicParkEdgeList = chainEdgeList flemingtonOlympicParkDists

-- Convert list of distances to first station to edges
chainEdgeList :: [Dist] ->  [Track]
chainEdgeList dists = map (\((x, y), (x', y')) -> (x, x', y' - y)) (chain [(fromEnum x, y) | (x, y) <- dists])

-- Generate pairs like handshakes at a party
handshakes :: [a] -> [(a, a)]
handshakes [] = []
handshakes (x:xs) = [(x, y) | y <- xs] ++ (handshakes xs)

-- Generate pairwise along the list
chain [x, y] = [(x, y)]
chain (x:y:xs) = [(x, y)] ++ chain (y:xs)
