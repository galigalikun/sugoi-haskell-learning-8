main :: IO ()

main = do
    putStrLn "A"
    let b = putStrLn "B"
    let c = putStrLn "C"
    c
    b
