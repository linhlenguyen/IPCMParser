module Processor.Lexer(
tokenise
)
where
  import Data.Map.Strict

  data Token = Compound | StringComparison | Organism | SearchParameter | String | Template | Invalid deriving(Show, Eq)

  getToken :: String -> Token
  getToken str = case getToken' str of {
    Just a -> a;
    Nothing -> Invalid;
  }

  --Improve by single pass tokeniser!
  tokenise :: [String] -> [(Token, String)]
  tokenise ls = reverse $ snd $ Prelude.foldl foldf ([],[]) ls
    where foldf :: (String,[(Token, String)]) -> String -> (String,[(Token, String)])
          foldf (sb,ac) str = if Prelude.null str then (sb,ac)
                              else if (head str == '"' && last str == '"') then ([], (String, init.tail $ str):ac)
                              else if (head str == '"' && Prelude.null sb) then (str,ac)
                              else if (not $ Prelude.null sb) then
                                if (last str == '"' || head str == '"') then ([],(String, init.tail $ (sb ++ " " ++ str)):ac)
                                else ((sb ++ " " ++ str),ac)
                              else ([],(getToken str, str):ac)

  getToken' :: String -> Maybe Token
  getToken' str = Data.Map.Strict.lookup str stringTokenMap

  stringTokenMap :: Map String Token
  stringTokenMap = fromList [("And", Compound),
    ("Or", Compound),
    ("Includes", StringComparison),
    ("Is", StringComparison),
    ("Is_not", StringComparison),
    ("Specimen", SearchParameter),
    ("Investigation", SearchParameter),
    ("Test_text", SearchParameter),
    ("Significant_isolates", Template),
    ("MRSA", Organism), ("CDT", Organism), ("ESBL", Organism),
    ("Tuberculosis", Organism), ("VRE", Organism), ("Group A streptococcus", Organism),
    ("Norovirus", Organism), ("Adenovirus", Organism), ("Rotavirus", Organism),
    ("H1N1", Organism), ("Varicella-zoster virus", Organism),
    ("MRSA bacteraemia", Organism), ("MSSA", Organism), ("E.coli", Organism),
    ("Campylobacter", Organism), ("Shigella", Organism), ("Salmonella", Organism),
    ("RSV", Organism), ("Gentamicin resistant organisms", Organism),
    ("CD-GDH", Organism), ("Influenza", Organism), ("CPE", Organism),
    ("MRSA Negative", Organism), ("AMPC", Organism)]
