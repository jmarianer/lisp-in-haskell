import Data.Char
import Data.List

-- Data type
data LispObject = Atom String | List [LispObject]

-- Output
instance Show LispObject where
  show (Atom a) = a
  show (List l) = "(" ++ (intercalate " " (map show l)) ++ ")"

showIndented :: LispObject -> String
showIndented = showIndented' ""
showIndented' indent (Atom a) = indent ++ a ++ "\n"
showIndented' indent (List l) = indent ++ "(\n" ++ concat (map (showIndented' (indent ++ "  ")) l) ++ indent ++ ")\n"

-- Input
parseLispObject :: String -> (LispObject, String)
parseLispObject (c:cs) = case c of
  '('  -> let (list, rest) = parseList cs                              in (List list, rest)
  _    -> let (atom, rest) = span (\c -> c /= ' ' && c /= ')') (c:cs)  in (Atom atom, rest)

parseList :: String -> ([LispObject], String)
parseList (c:cs) = case c of
  ' '  -> parseList (cs)
  ')'  -> ([], cs)
  _    -> let (firstElement, afterFirst) = parseLispObject (c:cs)
              (restOfList, restOfInput) = parseList afterFirst
          in  (firstElement:restOfList, restOfInput)
parseList "" = ([], "")

-- Very simple test
parseInput :: String -> LispObject
parseInput input = l
  where (l, _) = parseLispObject input
testInput = "(first (list 1 (+ 2 3) 9))"
main = do
  putStrLn ("Simple show: " ++ show object)
  putStrLn ("Indented show:\n" ++ showIndented' "  " object)
  where object = parseInput testInput
