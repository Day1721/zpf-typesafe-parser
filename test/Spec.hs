import Parser

-- TODO add some tests
tests :: [String]
tests = [
    "let x = 42 in ()"
    ]

testOne :: String -> IO ()
testOne code = return ()

main :: IO ()
main = putStrLn "Test suite not yet implemented" >> 
    mapM_ testOne tests
