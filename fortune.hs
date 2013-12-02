main = do
    putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn $ "Zis is your future: " ++ tellFortune name

-- 純粋なコード
tellFortune :: String -> String
tellFortune "Goro" = "good"
tellFortune name = "so so"
