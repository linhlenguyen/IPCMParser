{-# LANGUAGE GADTs #-}

module Data.IPCExp(
IPCExp(..),
toIPCXML
)
where
  import Data.Data
  import Data.Map.Strict
  import Data.IPCRuleXml

  data IPCExp where
    StringComparison :: RuleOperator -> SearchParameter -> SearchValue -> IPCExp
    Compound :: RuleOperator -> [IPCExp] -> IPCExp
    Template :: String -> SearchParameter -> IPCExp
    SignificantIsolate :: String -> IPCExp
    ID :: Int -> IPCExp -> IPCExp
    Organism :: Organism -> IPCExp -> IPCExp
    PID :: Int -> IPCExp -> IPCExp
    deriving (Show)

  toIPCXML :: Maybe String -> Maybe Int -> Maybe Int -> IPCExp -> [IPCRuleXml]
  toIPCXML rid pid organism (Compound operator ls) = IPCRuleXml {
    rule_id = rid,
    parent_rule_id = pid,
    organism_id = organism,
    rule_type_id = 1,
    rule_operator = Just $ operatorStringMap!operator,
    parameter_name = Nothing,
    parameter_value = Nothing,
    template = Nothing,
    template_input = Nothing
  } : (concatMap (toIPCXML (Just 0) organismid) ls)
  toIPCXML rid pid organism (StringComparison operator param value) = [IPCRuleXml {
    rule_id = rid,
    parent_rule_id = pid,
    organism_id = organism,
    rule_type_id = 2,
    rule_operator = Just $ operatorStringMap!operator,
    parameter_name = Just $ searchParameterMap!param,
    parameter_value = Just value,
    template = Nothing,
    template_input = Nothing
  }]
  toIPCXML rid pid organism (Template ttype value) = [IPCRuleXml {
    rule_id = rid,
    parent_rule_id = pid,
    organism_id = organism,
    rule_type_id = 5,
    rule_operator = Nothing,
    parameter_name = Nothing,
    parameter_value = Nothing,
    template = Just 0,
    template_input = Just value
  }]
  toIPCXML pid organismid (SignificantIsolate value) = [IPCRuleXml {
    rule_id = rid,
    parent_rule_id = pid,
    organism_id = organismid,
    rule_type_id = 2,
    rule_operator = Just "Includes",
    parameter_name = Just "Test_text",
    parameter_value = Just value,
    template = Nothing,
    template_input = Nothing
  }]

  --Significant isolates
