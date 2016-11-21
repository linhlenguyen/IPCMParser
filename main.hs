module Main(
main
)
where
  import System.Environment
  import Data.Data
  import Data.Map.Strict
  import Data.State
  import Data.ExpProcessor
  import Data.StringUtil
  import Data.IPCRuleParser

  mainLoop :: String -> IO ()
  mainLoop state = getLine >>= (\cmd -> case cmd of {
    "end" -> putStrLn "Terminated" >> putStrLn ("Text entered:" ++ state);
    a -> mainLoop (state ++ " " ++ a);
  })

  main :: IO ()
  main = do
    filePath <- getLine
    text <- readFile filePath
    tokenised <- return $ parseString False $ toWords text
    putStrLn $ show tokenised
    putStrLn $ show (parseTokens (keys organismIDMap) $ tokenised)

  --getLine >>= (\a -> getLine >>= (\c -> (getLine >>= (\b -> putStrLn "End")) >> putStrLn c) >> putStrLn a)
  --Program flow
  --Input -> IPCExp -> IPCRuleXml -> Xml

  --Sample data and test for export functions

  sampleData :: IPCConfiguration
  sampleData = IPCConfiguration {
    trust_code = "RNS",
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
