{-# LANGUAGE GADTs #-}

module Data.IPCExp(
IPCExp(..),
toIPCXML
)
where
  import Data.Data
  import Data.IPCRuleXml

  data IPCExp where
    StringComparison :: RuleOperator -> SearchParameter -> SearchValue -> IPCExp
    Compound :: RuleOperator -> [IPCExp] -> IPCExp
    Template :: String -> SearchParameter -> IPCExp
    deriving (Show)

  toIPCXML :: Maybe Int -> Maybe Int -> IPCExp -> [IPCRuleXml]
  toIPCXML pid organismid (Compound operator ls) = IPCRuleXml {
    rule_id = 0,
    parent_rule_id = pid,
    organism_id = organismid,
    rule_type_id = 1,
    rule_operator = Just operator,
    parameter_name = Nothing,
    parameter_value = Nothing,
    template = Nothing,
    template_input = Nothing
  } : (concatMap (toIPCXML (Just 0) organismid) ls)
  toIPCXML pid organismid (StringComparison operator param value) = [IPCRuleXml {
    rule_id = 0,
    parent_rule_id = pid,
    organism_id = organismid,
    rule_type_id = 2,
    rule_operator = Just operator,
    parameter_name = Just param,
    parameter_value = Just value,
    template = Nothing,
    template_input = Nothing
  }]
  toIPCXML pid organismid (Template ttype value) = [IPCRuleXml {
    rule_id = 0,
    parent_rule_id = pid,
    organism_id = organismid,
    rule_type_id = 5,
    rule_operator = Nothing,
    parameter_name = Nothing,
    parameter_value = Nothing,
    template = Just 0,
    template_input = Just value
  }]

  --Significant isolates
