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

  data InputState = First | IPCTemplate | Organism | CompoundOperator | ComparisonOperator | SearchParameter  deriving (Eq, Show)

  stringInputStateMap :: Map String InputState
  stringInputStateMap = fromList [("Significant_isolates", IPCTemplate)]

  type TemplateName = String
  type TemplateValue = String

  --Store IPC states
  data IPCConfiguration = IPCConfiguration{
      trust_code :: String,
      templates :: Map TemplateName [TemplateValue],
      rules :: Map Organism [IPCExp],
      current_organism :: Organism,
      input_state :: InputState,
      rule_counter :: Int
  } deriving (Show)

  -- initialState :: IPCConfiguration
  -- initialState = IPCConfiguration {
  --   trust_code = "Hello",
  --   rule_counter = 0
  -- }

  validateInput :: InputState -> String -> Bool
  validateInput state input@(x:xs) = case state of {
    IPCTemplate -> False; --Accept strings
    Organism -> False; --Accept CompoundOperator or ComparisonOperator
    CompoundOperator -> False; -- Accept CompoundOperator or ComparisonOperator
    ComparisonOperator -> False; -- Accept SearchParameter
    SearchParameter -> False; --Accept string as search Value
  }
