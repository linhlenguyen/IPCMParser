module Main(
main
)
where
  import System.Environment
  import Data.Data
  import Data.State
  import Data.ExpProcessor
  import Data.StringUtil
  import Data.IPCRuleParser
  import Processor.Lexer
  import Processor.Analyser
  import Data.ExpExporter

  mainLoop :: String -> IO ()
  mainLoop state = getLine >>= (\cmd -> case cmd of {
    "end" -> putStrLn "Terminated" >> putStrLn ("Text entered:" ++ state);
    a -> mainLoop (state ++ " " ++ a);
  })

  main :: IO ()
  main = do
    input <- getArgs
    let filePath = concat input
    text <- readFile filePath
    tokenised <- return $ exportRules $ getRulesForExport $ tokenToExp $ tokenise $ toWords text
    putStrLn $ tokenised
    writeFile "test2.xml" $ tokenised
    --putStrLn $ show (parseTokens (keys organismIDMap) $ tokenised)

  --getLine >>= (\a -> getLine >>= (\c -> (getLine >>= (\b -> putStrLn "End")) >> putStrLn c) >> putStrLn a)
  --Program flow
  --Input -> IPCExp -> IPCRuleXml -> Xml

  --Sample data and test for export functions
