module Processor.SemanticAnalyser(

)
where
  import Data.Data
  import Data.Map.Strict
  import Processor.Lexer
  import Data.IPCExp

  buildExpTree :: [(Token,String)] -> Map Tag [IPCExp]
  buildExpTree ls = undefined

  buildExp :: [(Token, String)] -> IPCExp
  buildExp ls = undefined

  checkToken :: [(Token, String)] -> Bool
  checkToken ls = any (\(t,s) -> t == Invalid) ls

  splitToken :: [(Token, String)] -> Map Tag [(Token, String)]
  splitToken ls = fromList $ foldf [] ls
    where foldf :: ([(Token,String)],[(Tag, [(Token,String)])]) -> (Token, String) -> ([(Token,String)],[(Tag, [(Token, String)])])
          foldf (ac,ls) ts = 
