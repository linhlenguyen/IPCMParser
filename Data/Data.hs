module Data.Data(
Organism,
organismIDMap,
RuleOperator,
operatorStringMap,
SearchParameter,
searchParameterMap,
SearchValue
)
  where
    import Data.Map.Strict

    type SearchValue = String
    type Organism = String
    organismIDMap :: Map Organism Int
    organismIDMap = fromList [("MRSA", 6), ("CDT", 7), ("ESBL", 8),
      ("Tuberculosis", 9), ("VRE", 10), ("Group A streptococcus", 11),
      ("Norovirus", 12), ("Adenovirus", 13), ("Rotavirus", 14),
      ("H1N1", 15), ("Varicella zoster virus", 16),
      ("MRSA bacteraemia", 17), ("MSSA", 22), ("E.coli", 23),
      ("Campylobacter", 24), ("Shigella", 25), ("Salmonella", 26),
      ("RSV", 27), ("Gentamicin resistant organisms", 28),
      ("CD-GDH", 29), ("Influenza", 34), ("CPE", 35),
      ("MRSA Negative", 37), ("AMPC", 38)]

    type RuleOperator = String
    operatorStringMap :: Map RuleOperator String
    operatorStringMap = fromList [("Is", "Like"), ("And", "And"), ("Or", "Or"),
      ("Includes", "Like")]

    type SearchParameter = String
    searchParameterMap :: Map SearchParameter String
    searchParameterMap = fromList [("Test_name", "TestName"),
      ("Test_text", "TestText"), ("Specimen", "SpecimenType"),
      ("Investigation", "InvestigationType;InvestigationDescription")]
