module Main(
main
)
where
  import System.Environment
  import Data.Data
  import Data.Map.Strict
  import Data.State
  import Data.SampleData
  import Data.StringUtil
  import Data.IPCRuleParser

  mainLoop :: String -> IO ()
  mainLoop state = getLine >>= (\cmd -> case cmd of {
    "end" -> putStrLn "Terminated" >> putStrLn ("Text entered:" ++ state);
    a -> mainLoop (state ++ " " ++ a);
  })

  main :: IO ()
  main = do
    filePath <- getLine
    text <- readFile filePath
    tokenised <- return $ parseString False $ toWords text
    putStrLn $ show tokenised
    putStrLn $ show (parseTokens (keys organismIDMap) $ tokenised)

  --getLine >>= (\a -> getLine >>= (\c -> (getLine >>= (\b -> putStrLn "End")) >> putStrLn c) >> putStrLn a)
  --Program flow
  --Input -> IPCExp -> IPCRuleXml -> Xml
