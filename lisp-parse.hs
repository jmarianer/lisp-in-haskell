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

-- REPL without the E :-)
handleLine :: String -> String
handleLine inputLine =
    "Simple show: " ++ show object ++ "\n" ++
    "Indented show:\n" ++ showIndented' "  " object
  where (object, _) = parseLispObject inputLine

main = do
  input <- getContents
  putStr (concatMap handleLine (lines input))
