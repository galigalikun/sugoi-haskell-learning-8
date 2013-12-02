makeQuestion :: IO ()
makeQuestion = putStrLn "Hello, what's your name?"

getAnswer :: IO String
getAnswer = getLine

printName :: String -> IO ()
printName name = putStrLn $ "Hey " ++ name ++ ", you rock!"

main :: IO ()
main = do
    makeQuestion
    name <- getAnswer
    printName name
