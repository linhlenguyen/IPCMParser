module Data.SampleData(
sampleData,
testExport,
writeToFile
)
where
  import Data.Data
  import Data.State
  import Data.IPCExp
  import Data.IPCRuleXml
  import Data.Map.Strict

  --Sample data and test for export functions

  sampleData :: IPCConfiguration
  sampleData = IPCConfiguration {
    trust_code = "",
    templates = fromList [("Significant_isolates",
      ["{0} DNA DECTECTED by PCR",
      "{0} ISOLATED",
      "{0} Identified",
      "{0} Positive",
      "Positive {0}"
      ])],
    rules = fromList [("Campylobacter",
                      [Compound "And" $ [StringComparison "Is" "Specimen" "Faeces",
                                         StringComparison "Includes" "Test_text" "Campylobacter PCR",
                                         Template "Significant_isolates" "Campylobacter jejuni"]]),
                      ("CD-GDH",
                      [StringComparison "Includes" "Test_text" "C.difficile Antigen"]),
                      ("MRSA",
                      [Template "Significant_isolates" "Staph. aureus(MRSA)"])],
    input_state = First,
    current_organism = "Adenovirus",
    rule_counter = 0
  }

  exports :: Map Organism [IPCExp] -> [String]
  exports x = undefined

  testExport :: [String]
  testExport = undefined --exportOrganism  (rules sampleData)

  exportOrganism :: String -> [IPCExp] -> [String]
  exportOrganism organismName exps = Prelude.map toXML (concatMap (toIPCXML (Just 0) (Just (organismIDMap!organismName))) $ exps)

  writeToFile :: String -> IO ()
  writeToFile fileName = writeFile fileName (Prelude.foldl (++) "" testExport)
