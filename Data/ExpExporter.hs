module Data.ExpExporter(
exportRules
)
  where
    import Data.Data
    import Data.State
    import Data.IPCRuleXml
    import Data.IPCExp
    import Data.List
    import Data.Map.Strict

    resolveInvalidXMLCharacters :: String -> String
    resolveInvalidXMLCharacters str = reverse $ Prelude.foldl foldf [] str
      where foldf s c = case c of { '&' -> ";pma&" ++ s;
                                    '"' -> ";43#&";
                                    a -> a:s; }

    exportXmlToString :: [IPCRuleXml] -> [String]
    exportXmlToString xml = Prelude.map toXML $ Data.List.sort xml

    exportOrganism :: [IPCExp] -> [IPCRuleXml]
    exportOrganism exps = (concatMap (toIPCXML (Nothing,Nothing,Nothing)) $ exps)

    --exportFromSample :: [IPCExp]
    --exportFromSample = getRulesForExport sampleData

    exportRules :: [IPCExp] -> String
    exportRules exprs = Prelude.foldl (++) "" $ exportXmlToString $ exportOrganism exprs

    --sampleRuleString :: [String]
    --sampleRuleString = exportXmlToString $ exportOrganism exportFromSample

    --writeToFile :: String -> IO ()
    --writeToFile fileName = writeFile fileName (Prelude.foldl (++) "" sampleRuleString)

    sampleData :: IPCConfiguration
    sampleData = IPCConfiguration {
      rules = Data.Map.Strict.fromList [("Significant_isolates", [Compound "Or" $ [SignificantIsolate "{0} DNA DECTECTED by PCR",
                        SignificantIsolate "{0} ISOLATED",
                        SignificantIsolate "{0} Identified",
                        SignificantIsolate "{0} Positive",
                        SignificantIsolate "Positive {0}"]]),
                        ("Campylobacter", [Compound "And" $ [StringComparison "Is" "Specimen" "^Faeces$",
                                                             Template "Significant_isolates" "Campylobacter (jejuni|PCR)"]]),
                        ("CD-GDH",[StringComparison "Includes" "Test_text" "C.difficile Antigen"]),
                        ("CDT", [StringComparison "Includes" "Test_text" "C.difficile Toxin A &amp; B"]),
                        ("CPE", [Compound "And" $ [StringComparison "Is_not" "Specimen" "^Blood culture$",
                                                   StringComparison "Includes" "Test_text" "CPE"]]),
                        ("ESBL", [StringComparison "Includes" "Test_text" "An extended spectrum beta-lactamase producer"]),
                        ("Influenza", [StringComparison "Includes" "Test_text" "Influenza (A|B) PCR"]),
                        ("MRSA", [Compound "And" $ [StringComparison "Is_not" "Specimen" "^Blood culture$",
                                                    Template "Significant_isolates" "Staph. aureus(MRSA)"]]),
                        ("Norovirus", [Compound "And" [StringComparison "Is" "Specimen" "^Faeces$",
                                                       Template "Significant_isolates" "Norovirus"]]),
                        ("Rotavirus", [StringComparison "Includes" "Test_text" "Rotavirus ELISA"]),
                        ("RSV", [StringComparison "Includes" "Test_text" "RSV(immunochromatographic)"]),
                        ("Salmonella", [Compound "And" [StringComparison "Is" "Specimen" "^Faeces$",
                                                        Template "Significant_isolates" "Salmonella (PCR)?"]]),
                        ("Shigella", [Compound "And" [StringComparison "Is" "Specimen" "^Faeces$",
                                                      Template "Significant_isolates" "Shigella/EIEC PCR"]]),
                        ("Tuberculosis", [Compound "Or" [Template "Significant_isolates" "Mycobacterium tuberculosis",
                                                         Template "Significant_isolates" "Acid-alcohol fast bacilli",
                                                         Template "Significant_isolates" "Mycobacterium sp."]]),
                        ("Varicella-zoster virus", [StringComparison "Includes" "Test_text" "varicella zoster virus"]),
                        ("VRE", [Compound "And" [Template "Significant_isolates" "Enterococcus",
                                                 StringComparison "Includes" "Test_text" "vancomycin resistant"]])
                        ]
    }
