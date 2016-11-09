module Main(
main
)
where
  import System.Environment
  import Data.Data
  import Data.Map.Strict
  import Data.State
  import Data.SampleData

  mainLoop :: String -> IO ()
  mainLoop state = getLine >>= (\cmd -> case cmd of {
    "end" -> putStrLn "Terminated" >> putStrLn ("Text entered:" ++ state);
    a -> mainLoop (state ++ " " ++ a);
  })

  main :: IO ()
  main = mainLoop ""

  --getLine >>= (\a -> getLine >>= (\c -> (getLine >>= (\b -> putStrLn "End")) >> putStrLn c) >> putStrLn a)
  --Program flow
  --Input -> IPCExp -> IPCRuleXml -> Xml
