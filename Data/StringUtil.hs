module Data.StringUtil(
split,
match,
match',
toLower,
toWords,
parseFile,
parseString
)
where
  parseFile :: FilePath -> IO ()
  parseFile filePath = do
    text <- readFile filePath
    putStrLn $ show (parseString False $ toWords text)

  data Token = String | Operator deriving (Show, Eq)

  parseQuote :: Token -> [String] -> [(Token,String)]
  parseQuote _ [] = []
  parseQuote t [x] = [(t,x)]
  parseQuote t (x:xs) = foldl foldingFnc [(t,[x])] xs
    where foldingFnc :: [(Token, String)] -> Char -> 

  parseString :: Bool -> [String] -> [String]
  parseString _ [] = []
  parseString _ [x] = x:[]
  parseString False (x:xs) = if (head x) == '"' then parseString True ((tail x) : xs)
                             else x : parseString False xs
  parseString True (x:xs) = if (last x) == '"' then (init x) : parseString False xs
                             else parseString True ((x ++ " " ++ (head xs)):(tail xs))

  toWords :: String -> [String]
  toWords (x:xs) = reverse $ map reverse $ foldl foldingFnc [[x]] xs
    where foldingFnc :: [String] -> Char -> [String]
          foldingFnc ls c = if any (\k -> k == c) [' ',',',';','\n','\r'] then
                              if (null $ head ls) then ls
                              else [] : ls
                            else (c : head ls) : tail ls

  split :: String -> String -> [String]
  split originalString splitString = undefined

  match :: String -> String -> Bool
  match _ [] = True
  match [] _ = True
  match (x:xs) (y:ys) = if x == y then match xs ys
                          else False

  match' :: String -> String -> Bool
  match' xs ys = match (toLower xs) (toLower ys)

  toLower :: String -> String
  toLower [] = []
  toLower (x:xs) = case x of {
    'A' -> 'a' : toLower xs;
    'B' -> 'b' : toLower xs;
    'C' -> 'c' : toLower xs;
    'D' -> 'd' : toLower xs;
    'E' -> 'e' : toLower xs;
    'F' -> 'f' : toLower xs;
    'G' -> 'g' : toLower xs;
    'H' -> 'h' : toLower xs;
    'I' -> 'i' : toLower xs;
    'J' -> 'j' : toLower xs;
    'K' -> 'k' : toLower xs;
    'L' -> 'l' : toLower xs;
    'M' -> 'm' : toLower xs;
    'N' -> 'n' : toLower xs;
    'O' -> 'o' : toLower xs;
    'P' -> 'p' : toLower xs;
    'Q' -> 'q' : toLower xs;
    'R' -> 'r' : toLower xs;
    'S' -> 's' : toLower xs;
    'T' -> 't' : toLower xs;
    'U' -> 'u' : toLower xs;
    'V' -> 'v' : toLower xs;
    'W' -> 'w' : toLower xs;
    'X' -> 'x' : toLower xs;
    'Y' -> 'y' : toLower xs;
    'Z' -> 'z' : toLower xs;
    c -> c : toLower xs;
  }
