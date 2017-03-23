module Processor.Lexer(
tokenise,
Token(..)
)
where
  import Data.Map.Strict

  data Token = TCompound | TStringComparison | TOrganism | TSearchParameter | TString | TTemplateHeader | TTemplate | TOpen | TClose | TInvalid deriving(Show, Eq)

  getToken :: String -> Token
  getToken str = case getToken' str of {
    Just a -> a;
    Nothing -> error $ "Invalid token found " ++ str;
  }

  --Improve by single pass tokeniser. Parse characters instead of strings
  tokenise :: [String] -> [(Token, String)]
  tokenise ls = reverse $ snd $ Prelude.foldl foldf ([],[]) ls
    where foldf :: (String,[(Token, String)]) -> String -> (String,[(Token, String)])
          foldf (sb,ac) str = if Prelude.null str then (sb,ac)
                              else if (head str == '"' && last str == '"') then ([], (TString, init.tail $ str):ac)
                              else if (head str == '"' && Prelude.null sb) then (str,ac)
                              else if (not $ Prelude.null sb) then
                                if (last str == '"' || head str == '"') then ([],(TString, init.tail $ (sb ++ " " ++ str)):ac)
                                else ((sb ++ " " ++ str),ac)
                              else ([],(getToken str, str):ac)

  getToken' :: String -> Maybe Token
  getToken' str = Data.Map.Strict.lookup str stringTokenMap

  stringTokenMap :: Map String Token
  stringTokenMap = fromList [("And", TCompound),
    ("Or", TCompound),
    ("Includes", TStringComparison),
    ("Is", TStringComparison),
    ("Is_not", TStringComparison),
    ("Specimen", TSearchParameter),
    ("Investigation", TSearchParameter),
    ("Test_text", TSearchParameter),
    ("Significant_isolates", TTemplate),
    ("MRSA", TOrganism), ("CDT", TOrganism), ("ESBL", TOrganism),
    ("Tuberculosis", TOrganism), ("VRE", TOrganism), ("Group_A_streptococcus", TOrganism),
    ("Norovirus", TOrganism), ("Adenovirus", TOrganism), ("Rotavirus", TOrganism),
    ("H1N1", TOrganism), ("Varicella-zoster_virus", TOrganism),
    ("MRSA bacteraemia", TOrganism), ("MSSA", TOrganism), ("E.coli", TOrganism),
    ("Campylobacter", TOrganism), ("Shigella", TOrganism), ("Salmonella", TOrganism),
    ("RSV", TOrganism), ("Gentamicin resistant organisms", TOrganism),
    ("CD-GDH", TOrganism), ("Influenza", TOrganism), ("CPE", TOrganism),
    ("MRSA Negative", TOrganism), ("AMPC", TOrganism),
    ("{", TOpen), ("}", TClose)]
