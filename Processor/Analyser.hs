module Processor.Analyser(
tokenToExp,
toTokenMap,
splitByOrganism,
extractExps
)
where
  import Data.Data
  import Data.Map.Strict
  import Processor.Lexer
  import Data.IPCExp

  checkToken :: [(Token, String)] -> Bool
  checkToken ls = any (\(t,s) -> t == TInvalid) ls

  topLevelTokens :: [Token]
  topLevelTokens = [TTemplate, TOrganism]

  splitByOrganism :: [(Token, String)] -> [[(Token, String)]]
  splitByOrganism (x:xs) = reverse $ Prelude.map reverse $ Prelude.foldl foldf [[x]] xs
    where foldf :: [[(Token, String)]] -> (Token, String) -> [[(Token, String)]]
          foldf ac ts@(t,x) = if t == TOrganism then [(t,x)] : ac
                           else (ts : (head ac)):(tail ac)

  toTokenMap :: [[(Token, String)]] -> Map Tag [(Token, String)]
  toTokenMap ls = fromList $ Prelude.map (\((t,s):xs) -> (s,xs)) ls

  tokenToExp :: [(Token, String)] -> Map Tag [IPCExp]
  tokenToExp ls = Data.Map.Strict.map (\x -> extractExps x) $ toTokenMap $ splitByOrganism ls

  extractExps :: [(Token, String)] -> [IPCExp]
  extractExps [] = []
  extractExps ((TCompound, op):xs) = (Compound op (extractExps xs)):[]
  extractExps ((TStringComparison, op):(TSearchParameter,sp):(TString,st):xs) = (StringComparison op sp st):(extractExps xs)
  extractExps ((TTemplate,tt):(TString,st):xs) = (Template tt st):(extractExps xs)
  extractExps _ = []
