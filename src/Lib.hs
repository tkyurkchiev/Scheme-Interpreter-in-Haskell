module Lib where

import Text.Parsec.Char (char, noneOf, oneOf, space)
import Text.ParserCombinators.Parsec
  ( Parser,
    between,
    many,
    many1,
    parse,
    (<|>),
  )

data Sexpr = NumExpr Integer | StringExpr String | Sexpr [Sexpr]
  deriving (Show)

type Env = [(String, Val)]

data Val
  = NumVal Integer
  | BoolVal Bool
  | Nil
  | List [Val]
  | StringVal String
  | Primitive ([Val] -> Env -> Val)
  | DefBind String Val
  | Error String
  | MacroVal ([Sexpr] -> Env -> Sexpr)
--sdasda
--instances
instance Show Val where
  show (NumVal n) = show n
  show (StringVal s) = s
  show (BoolVal b) = if b then "#t" else "#f"
  show Nil = "nil"
  show (List list) =
    case list of
      StringVal "quote" : _ -> "'" ++ unwordsL (tail list)
      _ -> "(" ++ unwordsL list ++ ")"
  show (Primitive _) = "<primitive>"
  show (Error s) = "Error: " ++ s
  show (DefBind s v) = s ++ " = " ++ show v
  show (MacroVal _) = "<macro>"

instance Eq Val where
  NumVal n == NumVal m = n == m
  BoolVal x == BoolVal y = x == y
  Nil == Nil = True
  List xs == List ys = xs == ys
  StringVal x == StringVal y = x == y
  _ == _ = False

--Parser Helpers
unwordsL :: [Val] -> String
unwordsL = unwords . map show

aDigit :: Parser Char
aDigit = oneOf ['0' .. '9']

digits :: Parser String
digits = many1 aDigit

symbol :: Parser Char
symbol = oneOf $ ['a' .. 'z'] ++ ['A' .. 'Z'] ++ "+-/!$#%;<=>?_&*@" ++ ['0' .. '9']

number :: Parser Sexpr
number = do NumExpr . read <$> digits

stringExpression :: Parser Sexpr
stringExpression = do
  char '"'
  s <- many (noneOf "\"")
  char '"'
  return $ StringExpr s

variable :: Parser Sexpr
variable = do
  first <- symbol <|> aDigit
  rest <- many symbol
  return $ StringExpr $ first : rest

sExpression :: Parser Sexpr
sExpression = do
  exp <- between (char '(') (char ')') (many1 expression)
  return $ Sexpr exp

atom :: Parser Sexpr
atom = number <|> stringExpression <|> variable <|> sExpression

parsedExpression :: Parser Sexpr
parsedExpression =
  atom
    <|> quotedExpression
    <|> quasiExpression
    <|> unquotedExpression

expHead :: Char -> String -> Parser Sexpr
expHead x rest = do
  char x
  exp <- many1 expression
  return $ Sexpr $ StringExpr rest : exp

quotedExpression :: Parser Sexpr
quotedExpression = expHead '\'' "quote"

quasiExpression :: Parser Sexpr
quasiExpression = expHead '`' "quasiquote"

unquotedExpression :: Parser Sexpr
unquotedExpression = expHead ',' "unquote"

expression :: Parser Sexpr
expression =
  parsedExpression <|> do
    many1 space
    parsedExpression

numOperator :: (Integer -> Integer -> Integer) -> [Val] -> Env -> Val
numOperator op (NumVal n : rest) env = NumVal $ foldl (\acc (NumVal x) -> op acc x) n rest
numOperator _ _ _ = Error "Not a number"

checkOrder :: (Integer -> Integer -> Bool) -> [Val] -> Env -> Val
checkOrder op (NumVal x : xs) env
  | null xs = BoolVal True
  | otherwise =
    case xs of
      NumVal y : ys -> if x `op` y then checkOrder op (NumVal y : ys) env else BoolVal False
      _ -> BoolVal False
checkOrder _ _ _ = Error "Not a number"

primitives =
  [ ("+", Primitive $ numOperator (+)),
    ("*", Primitive $ numOperator (*)),
    ("-", Primitive $ numOperator (-)),
    ("/", Primitive $ numOperator div),
    ("mod", Primitive $ numOperator mod),
    ("#t", BoolVal True),
    ("#f", BoolVal False),
    ("nil", Nil),
    ("cons", Primitive consFunc),
    ("car", Primitive car),
    ("cdr", Primitive cdr),
    ("=", Primitive $ checkOrder (==)),
    ("<", Primitive $ checkOrder (<)),
    ("<=", Primitive $ checkOrder (<=)),
    (">", Primitive $ checkOrder (>)),
    (">=", Primitive $ checkOrder (>=)),
    ("eq?", Primitive $ checkOrder (==)),
    ("eval", Primitive evaluate),
    ("list", Primitive listFunc),
    ("list?", Primitive isList),
    ("apply", Primitive applyPrimitive)
  ]

applyPrimitive :: [Val] -> Env -> Val
applyPrimitive [Primitive f, List args] env = f args env
applyPrimitive _ env = Error "Not a primitive"

isList :: [Val] -> Env -> Val
isList (List _ : _) _ = BoolVal True
isList _ _ = BoolVal False

listFunc :: [Val] -> Env -> Val
listFunc list env = List list

consFunc :: [Val] -> Env -> Val
consFunc [val, Nil] env = List [val]
consFunc [val, List list] env = List (val : list)
consFunc [val1, val2] env = List [val1, val2]
consFunc _ _ = Error "Wrong number of arguments"

evaluate :: [Val] -> Env -> Val
evaluate [exp] env = eval (unquotify exp) env
evaluate _ _ = Error "Wrong args"

car :: [Val] -> Env -> Val
car [List (x : _)] _ = x
car _ _ = error "Invalid arguments to car"

cdr :: [Val] -> Env -> Val
cdr [List (_ : xs)] _ = List xs
cdr _ _ = error "Invalid arguments to cdr"

eq :: [Val] -> Val
eq [x, y] = BoolVal $ x == y
eq _ = error "eq? takes two arguments"

cons :: [Val] -> Val
cons (x : [List xs]) = List $ x : xs
cons _ = error "cons takes two arguments"

list :: [Val] -> Val
list = List

splitList :: Int -> [a] -> [[a]]
splitList n = takeWhile (not . null) . unfoldr (Just . splitAt n)
  where
    unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
    unfoldr f x = case f x of
      Nothing -> []
      Just (a, b) -> a : unfoldr f b

unquotify :: Val -> Sexpr
unquotify (NumVal i) = NumExpr i
unquotify (StringVal name) = StringExpr name
unquotify (BoolVal b) = if b then StringExpr "#t" else StringExpr "#f"
unquotify (List terms) = Sexpr $ map unquotify terms
unquotify Nil = StringExpr "nil"
unquotify (DefBind name val) = Sexpr [StringExpr "def", StringExpr name, unquotify val]
unquotify (Error s) = StringExpr s
unquotify (Primitive _) = error "Cannot unquote primitives"
unquotify (MacroVal _) = error "Cannot unquote macros"

eval :: Sexpr -> Env -> Val
eval (NumExpr i) _ = NumVal i
eval (StringExpr s) ((var, val) : xs) = if s == var then val else eval (StringExpr s) xs
eval (StringExpr s) [] = Error $ "Unbound variable: " ++ s
eval (Sexpr [StringExpr "define", StringExpr name, value]) env = DefBind name (eval value env)
eval (Sexpr [StringExpr "cons", a, b]) env = cons [eval a env, eval b env]
eval (Sexpr [StringExpr "if", cond, true, false]) env =
  if eval cond env == BoolVal True then eval true env else eval false env
eval (Sexpr [StringExpr "quote", list]) _ = quote list
  where
    quote (NumExpr n) = NumVal n
    quote (StringExpr s) = StringVal s
    quote (Sexpr xs) = List $ map quote xs
eval (Sexpr (x : xs)) env = case eval x env of
  Primitive f -> f values env
    where
      values = map (`eval` env) xs
  DefBind name val -> eval (Sexpr [StringExpr "define", StringExpr name, Sexpr (x : xs)]) env
  Error s -> Error s
  _ -> Error "Invalid expression"
eval (Sexpr []) _ = Nil

evalInput :: String -> Env -> (Val, Env)
evalInput l env = case exp of
  Right e -> returnEnv $ eval e env
  Left e -> (Error (show e), env)
  where
    exp = parse expression "Expression" l
    returnEnv :: Val -> (Val, Env)
    returnEnv (DefBind name val) = (DefBind name val, (name, val) : env)
    returnEnv v = (v, env)

repl :: Env -> IO ()
repl env = do
  putStr "> "
  input <- getLine
  if input == ":q"
    then putStrLn "Exiting..."
    else do
      (val, newEnv) <- return $ evalInput input env
      print val
      repl newEnv

main :: IO ()
main = repl primitives