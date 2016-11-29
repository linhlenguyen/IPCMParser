{-# LANGUAGE GADTs #-}

module Data.IPCExp(
IPCExp(..),
toIPCXML,
setOrganism,
setParentId,
expCount,
expsCount
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
    TID :: Int -> SearchParameter -> IPCExp
    ExpID :: Int -> IPCExp -> IPCExp
    ExpOrganism :: Organism -> IPCExp -> IPCExp
    ExpPID :: Int -> IPCExp -> IPCExp
    deriving (Show)

  expsCount :: [IPCExp] -> Int
  expsCount ls = Prelude.foldl (\x expr -> x + expCount expr) 0 ls

  expCount :: IPCExp -> Int
  expCount (Compound op exprs) = 1 + expsCount exprs
  expCount (ExpID _ expr) = 1 + expCount expr
  expCount (ExpOrganism _ expr) = 1 + expCount expr
  expCount (ExpPID _ expr) = 1 + expCount expr
  expCount _ = 1

  --There must be a better way to box and unbox!
  type IPCContext = (Maybe Int, Maybe Int, Maybe String)
  toIPCXML :: IPCContext -> IPCExp -> [IPCRuleXml]
  toIPCXML (rid,pid,organism) (ExpID ruleId expr) = toIPCXML (Just ruleId, pid, organism) expr
  toIPCXML (rid,pid,organism) (ExpOrganism organismName expr) = toIPCXML (rid, pid, Just organismName) expr
  toIPCXML (rid,pid,organism) (ExpPID parentId expr) = toIPCXML (rid, Just parentId, organism) expr
  toIPCXML ctx expr = toIPCXML' (ctx,expr)

  setOrganism :: Organism -> IPCExp -> IPCExp
  setOrganism organism (Compound op ls) = (Compound op (Prelude.map (setOrganism organism) $ ls))
  setOrganism organism expr = (ExpOrganism organism expr)

  setParentId :: IPCExp -> IPCExp
  setParentId (ExpID pid expr) = (ExpID pid $ setParentId' pid expr)
  setParentId expr = expr

  setParentId' :: Int -> IPCExp -> IPCExp
  setParentId' pid (Compound op ls) = (Compound op (Prelude.map (setParentId'' pid) $ ls))
  setParentId' pid expr = expr

  setParentId'' :: Int -> IPCExp -> IPCExp
  setParentId'' pid (ExpID pid' (Compound op ls)) = (ExpPID pid (ExpID pid' (Compound op (Prelude.map (setParentId'' pid') $ ls))))
  setParentId'' pid expr = (ExpPID pid expr)

  toIPCXML' :: (IPCContext,IPCExp) -> [IPCRuleXml]
  toIPCXML' (ctx@(rid,pid,organism),(Compound operator ls)) = IPCRuleXml {
    rule_id = rid,
    parent_rule_id = pid,
    organism_id = organism,
    rule_type_id = 1,
    rule_operator = Just $ operatorStringMap!operator,
    parameter_name = Nothing,
    parameter_value = Nothing,
    template = Nothing,
    template_input = Nothing
  } : concatMap (toIPCXML ctx) ls
  toIPCXML' ((rid,pid,organism),(StringComparison operator param value)) = [IPCRuleXml {
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
  toIPCXML' ((rid,pid,organism),(SignificantIsolate value)) = [IPCRuleXml {
    rule_id = rid,
    parent_rule_id = pid,
    organism_id = organism,
    rule_type_id = 2,
    rule_operator = Just "Like",
    parameter_name = Just "TestValue",
    parameter_value = Just value,
    template = Nothing,
    template_input = Nothing
  }]
  toIPCXML' ((rid,pid,organism),(TID tid value)) = [IPCRuleXml {
    rule_id = rid,
    parent_rule_id = pid,
    organism_id = organism,
    rule_type_id = 5,
    rule_operator = Nothing,
    parameter_name = Nothing,
    parameter_value = Nothing,
    template = Just tid,
    template_input = Just value
  }]
  toIPCXML' (ctx,(Template ttype value)) = toIPCXML' (ctx,(TID 0 value))

  --Significant isolates
