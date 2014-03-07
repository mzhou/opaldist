import System.Environment (getArgs)

import Opal.Distance

main = do
    (orig:dest:_) <- getArgs
    putStrLn $ show $ trackDistance ((read orig) :: Station) ((read dest) :: Station)
