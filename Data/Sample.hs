module Data.Sample(
sampleData
)
where
  import Data.Map.Strict
  import Data.Data
  import Data.IPCExp

  exportFromSample :: [IPCExp]
  exportFromSample = undefined --getRulesForExport sampleData

  sampleRuleString :: [String]
  sampleRuleString = undefined --exportXmlToString $ exportOrganism exportFromSample

  writeToFile :: String -> IO ()
  writeToFile fileName = writeFile fileName (Prelude.foldl (++) "" sampleRuleString)

  sampleData :: Map Tag [IPCExp]
  sampleData = Data.Map.Strict.fromList [("Significant_isolates", [Compound "Or" $ [SignificantIsolate "{0} DNA DECTECTED by PCR",
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
