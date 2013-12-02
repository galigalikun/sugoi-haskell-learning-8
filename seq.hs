main = do
    a <- getLine
    b <- getLine
    c <- getLine
    print [a, b, c]

main' = do
    s <- sequence [getLine, getLine, getLine]
    print s
