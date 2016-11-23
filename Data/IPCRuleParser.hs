module Data.IPCRuleParser(
parseTokens
)
where
  import Data.Data
  import Data.Map.Strict
  import Data.IPCExp

  parseTokens :: [String] -> [String] -> [(String, [String])]
  parseTokens tokens ls = reverse $ Prelude.map (\(x,y) -> (x,reverse y)) $ Prelude.foldl foldingFnc [] ls
    where foldingFnc :: [(String, [String])] -> String -> [(String, [String])]
          foldingFnc ls str = if any (\o -> o == str) tokens then
                                (str,[]) : ls
                              else if (Prelude.null ls) then []
                                   else let (o,s) = head ls in
                                    (o,str:s) : tail ls

  parseRules :: [String] -> Map Tag [IPCExp]
  parseRules xs = fromList $ Prelude.foldl foldingFnc [] xs
    where foldingFnc :: [(Tag, [IPCExp])] -> String -> [(Tag, [IPCExp])]
          foldingFnc ls@((t,exprs):xs) ns = undefined

  parseOrganism :: String -> [String] -> ([String], [IPCExp])
  parseOrganism currentOrganism xs = Prelude.foldl foldingFnc ([],[]) xs
    where foldingFnc :: ([String], [IPCExp]) -> String -> ([String], [IPCExp])
          foldingFnc ac s = undefined
