module Data.StringUtil(
split,
match,
match',
toLower,
toWords
)
where

  toWords :: String -> [String]
  toWords (x:xs) = reverse $ map reverse $ foldl foldingFnc [[x]] xs
    where foldingFnc :: [String] -> Char -> [String]
          foldingFnc ls c = if any (\k -> k == c) [' ',',',';'] then
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
