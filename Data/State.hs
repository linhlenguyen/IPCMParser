module Data.State(
InputState(..),
IPCConfiguration(..)
)
where
  import Data.Data
  import Data.IPCExp
  import Data.Map.Strict
  --Input
  --Significant_isolates
  --"{0} DNA DETECTED by PCR"
  --"{0} ISOLATED"
  --"{0} Identified"
  --"{0} positive","positive {0}"
  --"(?<!not)positive"
  -- [("Significant_isolates",
  --   ["{0} DNA DECTECTED by PCR",
  --   "{0} ISOLATED",
  --   "{0} Identified",
  --   "{0} Positive",
  --   "Positive {0}"
  --   ])],
  --Campylobacter
  --And
  --Is Specimen "Faeces"
  --Or
  --Includes Test_text "Camplylobacter PCR"
  --Significant_isolates "Campylobacter jejuni"

  --CD-GDH
  --Includes Test_text "C.difficile Antigen"

  --Tuberculosis
  --Or
  --Siginificant_isolates "Mycobacterium tuberculosis"

  data InputState = First | IPCTemplate | CompoundOperator | ComparisonOperator | SearchParameter  deriving (Eq, Show)

  stringInputStateMap :: Map String InputState
  stringInputStateMap = fromList [("Significant_isolates", IPCTemplate)]

  type RuleTag = String
  --Store IPC states
  data IPCConfiguration = IPCConfiguration{
      rules :: Map RuleTag [IPCExp]
  } deriving (Show)

  validateInput :: InputState -> String -> Bool
  validateInput state input@(x:xs) = case state of {
    IPCTemplate -> False; --Accept strings
    CompoundOperator -> False; -- Accept CompoundOperator or ComparisonOperator
    ComparisonOperator -> False; -- Accept SearchParameter
    SearchParameter -> False; --Accept string as search Value
  }
