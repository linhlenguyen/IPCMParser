module Data.SampleData(
sampleData,
writeToFile
)
where
  import Data.Data
  import Data.State
  import Data.IPCExp
  import Data.IPCRuleXml
  import Data.Map.Strict
  import Data.List

  --Sample data and test for export functions

  sampleData :: IPCConfiguration
  sampleData = IPCConfiguration {
    trust_code = "RNS",
    rules = fromList [("Significant_isolates", [ExpID 1 $ Compound "Or" $ [ExpID 2 $ SignificantIsolate "{0} DNA DECTECTED by PCR",
                      ExpID 3 $ SignificantIsolate "{0} ISOLATED",
                      ExpID 4 $ SignificantIsolate "{0} Identified",
                      ExpID 5 $ SignificantIsolate "{0} Positive",
                      ExpID 6 $ SignificantIsolate "Positive {0}"]]),
                      ("Campylobacter", [ExpID 7 $ Compound "And" $ [ExpID 8 $ StringComparison "Is" "Specimen" "Faeces",
                                                                     ExpID 9 $ StringComparison "Includes" "Test_text" "Campylobacter PCR",
                                                                     ExpID 10 $ Template "Significant_isolates" "Campylobacter jejuni"]]),
                      ("CD-GDH",[ExpID 11 $ StringComparison "Includes" "Test_text" "C.difficile Antigen"]),
                      ("CDT", [ExpID 12 $ StringComparison "Includes" "Test_text" "C.difficile Toxin A & B"]),
                      ("CPE", [ExpID 13 $ Compound "And" $ [ExpID 14 $ StringComparison "Is_not" "Specimen" "^Blood culture$",
                                                            ExpID 15 $ StringComparison "Includes" "Test_text" "CPE"]]),
                      ("ESBL", [ExpID 14 $ StringComparison "Includes" "Test_text" "An extended spectrum beta-lactamase producer"]),
                      ("Influenza", [ExpID 15 $ StringComparison "Includes" "Test_text" "Influenza (A|B) PCR"]),
                      ("MRSA", [ExpID 16 $ Compound "And" $ [ExpID 17 $ StringComparison "Is_not" "Specimen" "^Blood culture$",
                                                             ExpID 18 $ Template "Significant_isolates" "Staph. aureus(MRSA)"]]),
                      ("Norovirus", [ExpID 19 $ Compound "And" [ExpID 20 $ StringComparison "Is" "Specimen" "^Faeces$",
                                                                ExpID 21 $ StringComparison "Includes" "Test_text" "Norovirus"]]),
                      ("Rotavirus", [ExpID 22 $ StringComparison "Includes" "Test_text" "Rotavirus ELISA"]),
                      ("RSV", [ExpID 23 $ StringComparison "Includes" "Test_text" "RSV(immunochromatographic)"]),
                      ("Salmonella", [ExpID 24 $ Compound "And" [ExpID 25 $ StringComparison "Is" "Specimen" "Faeces",
                                                                ExpID 26 $ Compound "Or"[ExpID 27 $ Template "Significant_isolates" "Salmonella",
                                                                                         ExpID 28 $ StringComparison "Includes" "Test_text" "Salmonella PCR"]]]),
                      ("Shigella", [ExpID 29 $ Compound "And" [ExpID 30 $ StringComparison "Is" "Specimen" "Faeces",
                                                               ExpID 31 $ StringComparison "Includes" "Test_text" "Shigella/EIEC PCR"]]),
                      ("Tuberculosis", [ExpID 32 $ Compound "Or" [ExpID 33 $ Template "Significant_isolates" "Mycobacterium tuberculosis",
                                                                  ExpID 34 $ Template "Significant_isolates" "Acid-alcohol fast bacilli",
                                                                  ExpID 35 $ Template "Significant_isolates" "Mycobacterium sp."]]),
                      ("Varicella-zoster virus", [ExpID 36 $ StringComparison "Includes" "Test_text" "varicella zoster virus"]),
                      ("VRE", [ExpID 37 $ Compound "And" [ExpID 38 $ Template "Significant_isolates" "Enterococcus",
                                                          ExpID 39 $ StringComparison "Includes" "Test_text" "vancomycin resistant"]])
                      ]
  }

  resolveTemplates :: Map Tag [IPCExp] -> Map Tag [IPCExp]
  resolveTemplates r = r

  getRulesForExport :: IPCConfiguration -> [IPCExp]
  getRulesForExport ipcConfig = concat $ Data.Map.Strict.elems $ resolveTemplates.resolveOrganism.resolvePID $ rules ipcConfig

  resolvePID :: Map Tag [IPCExp] -> Map Tag [IPCExp]
  resolvePID exprs = Data.Map.Strict.map (\v ->Prelude.map setParentId $ v) exprs

  resolveOrganism :: Map Tag [IPCExp] -> Map Tag [IPCExp]
  resolveOrganism exprs = Data.Map.Strict.mapWithKey mappingFnc exprs
    where mappingFnc :: Tag -> [IPCExp] -> [IPCExp]
          mappingFnc k ls = if isOrganism k then (Prelude.map (setOrganism k) $ ls) else ls
          isOrganism :: Tag -> Bool
          isOrganism k = elem k $ Data.Map.Strict.keys organismIDMap

  exportXmlToString :: [IPCRuleXml] -> [String]
  exportXmlToString xml = Prelude.map toXML $ Data.List.sort xml

  exportOrganism :: [IPCExp] -> [IPCRuleXml]
  exportOrganism exps = (concatMap (toIPCXML (Nothing,Nothing,Nothing)) $ exps)

  exportFromSample :: [IPCExp]
  exportFromSample = getRulesForExport sampleData

  sampleRuleString :: [String]
  sampleRuleString = exportXmlToString $ exportOrganism exportFromSample

  writeToFile :: String -> IO ()
  writeToFile fileName = writeFile fileName (Prelude.foldl (++) "" sampleRuleString)
