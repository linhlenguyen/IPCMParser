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
    trust_code = "RNS",
    rules = fromList [("Significant_isolates",
                      [ID 1 $ Compound "Or" $ [ID 2 $ SignificantIsolate "{0} DNA DECTECTED by PCR",
                      ID 3 $ SignificantIsolate "{0} ISOLATED",
                      ID 4 $ SignificantIsolate "{0} Identified",
                      ID 5 $ SignificantIsolate "{0} Positive",
                      ID 6 $ SignificantIsolate "Positive {0}"]]),
                      ("Campylobacter",
                      [ID 7 $ Compound "And" $ [ID 8 $ StringComparison "Is" "Specimen" "Faeces",
                                         ID 9 $ StringComparison "Includes" "Test_text" "Campylobacter PCR",
                                         ID 10 $ Template "Significant_isolates" "Campylobacter jejuni"]]),
                      ("CD-GDH",
                      [ID 11 $ StringComparison "Includes" "Test_text" "C.difficile Antigen"]),
                      ("MRSA",
                      [ID 12 $ Template "Significant_isolates" "Staph. aureus(MRSA)"])]
  }

  insertIDs :: Map RuleTag [IPCExp] -> Map RuleTag [IPCExp]
  insertIDs ruleMap = Data.Map.Strict.map mappingFnc ruleMap
    where mappingFnc (x:xs) -> foldl ()

  testExport :: [String]
  testExport = undefined

  exportOrganism :: String -> [IPCExp] -> [String]
  exportOrganism organismName exps = Prelude.map toXML (concatMap (toIPCXML (Just 0) (Just (organismIDMap!organismName))) $ exps)

  writeToFile :: String -> IO ()
  writeToFile fileName = writeFile fileName (Prelude.foldl (++) "" testExport)
