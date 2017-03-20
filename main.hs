module Main(
main
)
where
  import System.Environment
  import Data.IPCExp
  import Data.Data
  import Data.StringUtil
  import Processor.Lexer
  import Processor.Analyser
  import Processor.ExpProcessor
  import Processor.ExpExporter
  import Data.Sample

  main :: IO ()
  main = do
    filePath <- getLine
    text <- readFile filePath
    tokenised <- return $ exportRules $ getRulesForExport $ tokenToExp $ tokenise $ toWords text
    writeFile "test.xml" $ tokenised
