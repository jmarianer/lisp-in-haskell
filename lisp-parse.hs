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

-- Evaluate
data LispValue = Number Int | Function ([LispValue] -> LispValue) | ValueList [LispValue]
instance Show LispValue where
  show (Number i) = show i
  show (Function _) = "A function"
  show (ValueList l) = "(" ++ (intercalate " " (map show l)) ++ ")"

fromNumber :: LispValue -> Int
fromNumber (Number i) = i

fromFunction :: LispValue -> [LispValue] -> LispValue
fromFunction (Function f) = f

eval :: LispObject -> LispValue
eval (Atom "+") = Function (\values -> Number (sum (map fromNumber values)))
eval (Atom "list") = Function (\list -> ValueList list)
eval (Atom "car") = Function (\[ValueList (l:_)] -> l)
eval (Atom string) = Number (read string)
eval (List (function:params)) = (fromFunction (eval function)) (map eval params)

-- REPL without the E :-)
handleLine :: String -> String
handleLine inputLine =
    "Simple show: " ++ show object ++ "\n" ++
    "Indented show:\n" ++ showIndented' "  " object ++
    "Evaluated: " ++ show (eval object) ++ "\n\n"
  where (object, _) = parseLispObject inputLine


main = do
  input <- getContents
  putStr (concatMap handleLine (lines input))
