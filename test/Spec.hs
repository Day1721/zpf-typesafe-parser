import Parser
import Text.Megaparsec
import Parser
import TypeChecking.TC
import System.Exit

-- TODO add some tests
tests :: [(String, String)]
tests = [
    ("test1" , "let x = 42 in ()")
    ]

testOne :: String -> String -> IO ()
testOne name code = case runParsing name code of
    Left err -> {-putStrLn err-} testParsing code >> exitWith (ExitFailure 1)
    Right _ -> return ()

main :: IO ()
main = putStrLn "Test suite not yet implemented" >> 
    mapM_ (uncurry testOne) tests
