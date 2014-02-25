import System.Environment (getArgs)

import Opal.Distance.Stations (Station)
import Opal.Distance.Calc (fareDistance)

main = do
    (orig:dest:_) <- getArgs
    putStrLn $ show $ fareDistance ((read orig) :: Station) ((read dest) :: Station)
