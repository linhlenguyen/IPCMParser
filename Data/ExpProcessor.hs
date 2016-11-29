module Data.ExpProcessor(
getRulesForExport
)
where
  import Data.Data
  import Data.State
  import Data.IPCExp
  import Data.IPCRuleXml
  import Data.Map.Strict
  import Data.Map
  import Data.List

  --Traverse through the tree of expressions and give expression a unique ID
  --Speed things up by embedded expCount to return data (expsCount is not required)
  resolveID :: Map Tag [IPCExp] -> Map Tag [IPCExp]
  resolveID ipcr = Data.Map.Strict.fromList $ snd $ Data.Map.foldlWithKey foldingFnc (0,[]) ipcr
    where foldingFnc :: (Int,[(Tag,[IPCExp])]) -> Tag -> [IPCExp] -> (Int,[(Tag,[IPCExp])])
          foldingFnc (i,ac) t exprs = let ni = i + (expsCount exprs) in
                                        (ni,(t, resolveIDExps i exprs) : ac)

  resolveIDExps :: Int -> [IPCExp] -> [IPCExp]
  resolveIDExps si exprs = snd $ Data.List.foldl foldf (si,[]) exprs
    where foldf :: (Int,[IPCExp]) -> IPCExp -> (Int,[IPCExp])
          foldf (i,ls) nexpr = (i + expCount nexpr,(resolveIDExp i nexpr):ls)

  resolveIDExp :: Int -> IPCExp -> IPCExp
  resolveIDExp i (Compound op exprs) = ExpID i $ (Compound op (resolveIDExps (i+1) exprs))
  resolveIDExp i expr = (ExpID i expr)

  resolveTemplates :: Map Tag [IPCExp] -> Map Tag [IPCExp]
  resolveTemplates ipcr = Data.Map.Strict.map (Prelude.map mappingFnc) ipcr
    where getTemplateExp :: String -> Maybe [IPCExp]
          getTemplateExp str = Data.Map.Strict.lookup str ipcr
          getTemplateId :: Maybe [IPCExp] -> Int
          getTemplateId (Just ((ExpID pid _):xs)) = pid
          getTemplateId _ = 0
          mappingFnc :: IPCExp -> IPCExp
          mappingFnc (Template tString sText) = (TID (getTemplateId (getTemplateExp tString)) sText)
          mappingFnc (Compound op exprs) = (Compound op (Prelude.map mappingFnc exprs))
          mappingFnc (ExpID eid expr) = (ExpID eid (mappingFnc expr))
          mappingFnc expr = expr

  getRulesForExport :: Map Tag [IPCExp] -> [IPCExp]
  getRulesForExport exprs = concat $ Data.Map.Strict.elems $ resolveOrganism.resolvePID.resolveTemplates.resolveID $ exprs

  resolvePID :: Map Tag [IPCExp] -> Map Tag [IPCExp]
  resolvePID exprs = Data.Map.Strict.map (\v ->Prelude.map setParentId $ v) exprs

  resolveOrganism :: Map Tag [IPCExp] -> Map Tag [IPCExp]
  resolveOrganism exprs = Data.Map.Strict.mapWithKey mappingFnc exprs
    where mappingFnc :: Tag -> [IPCExp] -> [IPCExp]
          mappingFnc k ls = if isOrganism k then (Prelude.map (setOrganism k) $ ls) else ls
          isOrganism :: Tag -> Bool
          isOrganism k = elem k $ Data.Map.Strict.keys organismIDMap
