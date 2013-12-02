import Control.Monad
import Data.Char

main = do
    input <- getLine
    when (map toUpper input == "SWORDFISH") $ do
        putStrLn input

main' = do
    input <- getLine
    if (map toUpper input == "SWORDFISH")
        then putStrLn input
        else return ()
