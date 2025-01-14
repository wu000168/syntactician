import Paths_grammar (getDataFileName)

main :: IO ()
main = do 
    dataFile <- getDataFileName "resources/examples/eng.grammar"
    print =<< readFile dataFile

