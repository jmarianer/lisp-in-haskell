import System.Console.Haskeline
import Data.Char
import Data.List
import qualified Data.Map as Map

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
type Environment = Map.Map String LispValue
defaultEnv :: Environment
defaultEnv = Map.fromList [
  ("+",    Function (\values -> Number (sum (map fromNumber values)))),
  ("list", Function (\list -> ValueList list)),
  ("car",  Function (\[ValueList (l:_)] -> l)),
  ("pi",   Number 3)
  ]

data LispValue = Number Int | Function ([LispValue] -> LispValue) | ValueList [LispValue]
instance Show LispValue where
  show (Number i) = show i
  show (Function _) = "A function"
  show (ValueList l) = "(" ++ (intercalate " " (map show l)) ++ ")"

fromNumber :: LispValue -> Int
fromNumber (Number i) = i

fromFunction :: LispValue -> [LispValue] -> LispValue
fromFunction (Function f) = f

eval :: Environment -> LispObject -> LispValue
eval env (Atom string) = 
  if isDigit (head string)
  then Number (read string)
  else env Map.! string

eval env (List [Atom "let", List initializers, expression]) =
  let
    [Atom name, value] = initializers
    env' = Map.insert name (eval env value) env
  in eval env' expression
eval env (List (function:params)) = (fromFunction (eval env function)) (map (eval env) params)

-- Simple REPL
handleLine :: String -> String
handleLine inputLine =
    "Simple show: " ++ show object ++ "\n" ++
    "Indented show:\n" ++ showIndented' "  " object ++
    "Evaluated: " ++ show (eval defaultEnv object) ++ "\n"
  where (object, _) = parseLispObject inputLine


main :: IO ()    -- Copied from Haskeline documentation
main = runInputT defaultSettings {historyFile = Just ".lisp_history"} loop
 where
   loop :: InputT IO ()
   loop = do
     minput <- getInputLine "> "
     case minput of
       Nothing -> return ()
       Just input -> do outputStrLn $ handleLine input
                        loop

