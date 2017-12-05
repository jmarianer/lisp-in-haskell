import Data.Char
import Data.List

data List = Atom String | List [List]

testInput = "(first (list 1 (+ 2 3) 9))"

parseList :: String -> ([List], String)
parseList (c:cs) = case c of
  ' '  -> parseList (cs)
  ')'  -> ([], cs)
  _    -> let (firstElement, afterFirst) = parseListOrAtom
              (restOfList, restOfInput) = parseList afterFirst
          in  (firstElement:restOfList, restOfInput)
  where
    parseListOrAtom :: (List, String)
    parseListOrAtom = case c of
      '('  -> let (list, rest) = parseList cs                              in (List list, rest)
      _    -> let (atom, rest) = span (\c -> c /= ' ' && c /= ')') (c:cs)  in (Atom atom, rest)
parseList "" = ([], "")


parseInput :: String -> List
parseInput input = l
  where (l:_, _) = parseList input

instance Show List where
  show (Atom a) = a
  show (List l) = "(" ++ (intercalate " " (map show l)) ++ ")"

main = putStrLn $ show $ parseInput testInput
