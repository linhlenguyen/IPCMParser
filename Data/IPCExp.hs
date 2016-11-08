{-# LANGUAGE GADTs #-}

module Data.IPCExp(
IPCExp(..)
)
where
  import Data.Data
  import Data.IPCRuleXml

  data IPCExp where
    StringComparison :: RuleOperator -> SearchParameter -> SearchValue -> IPCExp
    Compound :: RuleOperator -> [IPCExp] -> IPCExp
    Template :: Int -> SearchParameter -> IPCExp
    deriving (Show)

  toIPCXML :: IPCExp -> [IPCRuleXml]
  toIPCXML = undefined
