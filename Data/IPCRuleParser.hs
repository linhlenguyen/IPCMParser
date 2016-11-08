module IPCRuleParser(
parseOrganism,
parseOperator,
parseRule
)
where
  import Data.Data
  import Data.IPCRule

  parseOrganism :: String -> [IPCRule]
  parseOrganism = undefined

  parseOperator :: String -> [IPCRule]
  parseOperator = undefined

  --Example String
  --Specimen is faeces
  --Significant isolates Mycobacterium tuberculosis ISOLATED
  --Test text includes C.difficile Toxin A & B
  --Test name is Culture
  parseRule :: String -> [IPCRule]
  parseRule = undefined
