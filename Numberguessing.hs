import System.IO
import System.Random
import System.Environment

main            :: IO ()
main            = do args <- getArgs                            -- get arguments
                    let (min, max) = safeHead2 0 100 args       -- first 2 args are min and max
                    number <- randomRIO (min, max)              -- generate random number