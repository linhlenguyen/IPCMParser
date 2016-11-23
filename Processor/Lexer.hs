module Processor.Lexer(
tokeniser
)
where
  import Data.Map.Strict

  data Token = Compound | StringComparison | Organism | SearchParameter | String | Template | Other deriving(Show, Eq)

  tokeniser :: [String] -> [(Token, String)]
  tokeniser ls@(x:xs) = reverse $ Prelude.map (\(x,y) -> (x, reverse y)) $ snd $ foldl fn (Other,[]) ls
    where fn :: (Token,[(Token, String)]) -> String -> (Token,[(Token, String)])
          fn (t,ac) str = undefined


  getToken :: String -> Maybe Token
  getToken str = lookup stringTokenMap str

  stringTokenMap :: Map String Token
  stringTokenMap = fromList [
  ("And", Compound),
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
