module Processor.ExpExporter(
exportRules
)
  where
    import Data.Data
    import Data.IPCRuleXml
    import Data.IPCExp
    import Data.List
    import Data.Map.Strict

    resolveInvalidXMLCharacters :: String -> String
    resolveInvalidXMLCharacters str = reverse $ Prelude.foldl foldf [] str
      where foldf s c = case c of { '&' -> ";pma&" ++ s;
                                    '"' -> ";43#&";
                                    a -> a:s; }

    exportXmlToString :: [IPCRuleXml] -> [String]
    exportXmlToString xml = Prelude.map toXML $ Data.List.sort xml

    exportOrganism :: [IPCExp] -> [IPCRuleXml]
    exportOrganism exps = (concatMap (toIPCXML (Nothing,Nothing,Nothing)) $ exps)

    exportRules :: [IPCExp] -> String
    exportRules exprs = Prelude.foldl (++) "" $ exportXmlToString $ exportOrganism exprs
