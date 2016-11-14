module Data.IPCRuleParser(
parseTokens
)
where
  import Data.Data
  import Data.Map.Strict

  parseTokens :: [String] -> [String] -> [(String, [String])]
  parseTokens tokens ls = Prelude.foldl foldingFnc [] ls
    where foldingFnc :: [(String, [String])] -> String -> [(String, [String])]
          foldingFnc ls str = if any (\o -> o == str) tokens then
                                (str,[]) : ls
                              else let (o,s) = head ls in
                                   (o,str:s) : tail ls
