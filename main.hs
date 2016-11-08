module Main(
main
)
where
  import System.Environment
  import Data.Data
  import Data.Map.Strict
  import Data.State

  mainLoop :: IPCConfiguration -> IO ()
  mainLoop state = undefined
    --do
    --args <- getArgs

  mockParser :: String -> IO String
  mockParser "Hello" = return "The Infinite world of endless patterns"

  main :: IO ()
  main = getLine >>= (\cmd -> case cmd of {
    "end" -> putStrLn "Program terminated";
    a -> main >>= (\b -> putStrLn (b ++ a));
  })

  --Program flow
  --Input -> IPCExp -> IPCRuleXml -> Xml
