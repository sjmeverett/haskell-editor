
module FoulParser where

import Data.Char
import Data.List
import Control.Applicative
import Data.Traversable
import FOUL

{----
(0) IDENTIFY YOURSELF

Name: Stewart MacKenzie-Leigh

----}

newtype Parser x =
  Parser {parse :: String -> Either (String, String) (x, String)}

{----
(1) Define suitable instances of Applicative and Alternative to
support parsing. Use

  Left (e, s)   to indicate that input s was rejected with message e
  Right (x, s)  to indicate that x was successfully delivered, with
                  s the remainder of the input
----}

instance Applicative Parser where
    -- pure :: s -> Parser s
    pure x = Parser $ \s -> Right (x, s)
    
    -- (<*>) :: f (s -> t) -> f s -> f t
    -- f :: String -> Either (String, String) (s -> t, String)
    -- x :: String -> Either (String, String) (s, String)
    Parser f <*> Parser x = Parser $ \s -> case f s of
        Right (f, s) -> case x s of
            Right (x, s) -> Right (f x, s)
            Left x -> Left x
        Left x -> Left x


instance Alternative Parser where
    -- empty :: f s
    empty = Parser $ \s -> Left ("", s)
    
    -- (<|>) :: f a -> f a -> f a
    Parser a <|> Parser b = Parser $ \str -> case a str of
        Right x -> Right x
        Left (e, s) -> case b str of
            Right x -> Right x
            Left (e', t)
                | length s < length t -> Left (t, e')
                | otherwise -> Left (s, e)
    
    
  -- empty should reject the input with no message
  -- <|> should succeed if possible; if both arguments fail,
  --   some attempt should be made to choose the more reasonable
  --   explanation (how might the length of the rejected input
  --   play a part? how might the message?)

{- I've provided a Functor instance for you: -}

instance Functor Parser where
  fmap = (<*>) . pure

{- Define a parser for a single character which must satisfy a given test. -}

char :: (Char -> Bool) -> Parser Char
char f = Parser match where
    match (c:cs) = case f c of
        True -> Right (c, cs)
        _ -> Left ("", c:cs)
    match [] = Left ("", [])
    
    
eof :: Parser ()
eof = Parser $ \s -> case s of
    [] -> Right((), [])
    _ -> Left ("", s)

{----
What does

  traverse (char . (==))

do, e.g., if you apply it to "fred"?
----}

{----
Define a parser which replaces the empty error message with a better one.
----}

message :: String -> Parser x -> Parser x
message msg (Parser p) = Parser $ \s -> case p s of
    Right x -> Right x
    Left ("", s) -> Left (msg, s)
    Left x -> Left x

{----
You should make sure that

  message "barf" empty <|> empty == empty <|> message "barf" empty

and that both deliver the message "barf" rather than no message at all.
----}

{----
(2) Define parsers for tokens

Hint: you may find it useful to check out the operators

  many
  some

in the library documentation for Control.Applicative.
----}

space :: Parser ()
space = () <$ many (char isSpace)

variable :: Parser String
variable = message "expected identifier" 
         $ (:) <$ space <*> char isLower <*> many (char isAlphaNum)
         
  -- this should accept any sequence of alphanumeric characters
  -- beginning with a lower case letter

constructor :: Parser String
constructor = message "expected constructor" 
            $ (:) <$ space <*> char isUpper <*> many (char isAlphaNum)
  -- this should accept any sequence of alphanumeric characters
  -- beginning with an upper case letter

{----
(3) Define a parser combinator for comma-separated lists in parentheses
----}

parenList :: Parser x -> Parser [x]
parenList p = message "expected parenthesised list" 
            $ space *> char (=='(') *> list p <* space <* char (==')')

list :: Parser x -> Parser [x]
list p = ((:) <$> p <*> many clause) <|> pure [] where
    clause = space *> char (==',') *> p
    
  -- this should parse string (s1,s2,...,sn) as
  -- list [x1,x2,...,xn], if p parses each s as the corresponding x;
  -- be as liberal as you can about extra whitespace

{----
(4) Define parsers for FOUL expressions and patterns
----}

{--- remember
data Expr
  = EC String [Expr]  -- should look like    Con(e1,...,en)
  | EV String        -- should look like    x
  | EA String [Expr]  -- should look like    f(e1,...en)
  deriving Show
----}

parseExpr :: Parser Expr
parseExpr = message "expected expression"
         $  (EC <$> constructor <*> (parenList parseExpr <|> pure []))
        <|> (EA <$> variable <*> parenList parseExpr)
        <|> (EV <$> variable)

{---- remember
data Pat
  = PC String [Pat]  -- should look like    Con(e1,...,en)
  | PV String        -- should look like    x
  deriving Show
----}

parsePat :: Parser Pat
parsePat = message "expected pattern"
        $  (PC <$> constructor <*> (parenList parsePat <|> pure []))
       <|> (PV <$> variable)

{----
(5) Define parsers for FOUL Line and whole FOUL programs
----}

{---- remember
type Line = ([Pat], Expr)
----}

parseLine :: Parser Line
parseLine = message "expected function line"
          $ (,) <$> parenList parsePat <* space <* char (=='=') <*> parseExpr

{---- remember
type Prog = [(String, [Line])]
----}

parseProg :: Parser Prog
parseProg = message "expected function definition or end of file"
          $ (groupFuncs <$> many parseFunc) <* space <* eof
--parseProg = (\(s, l) -> [(s, [l])]) <$> parseFunc

groupFuncs :: [(String, Line)] -> [(String, [Line])]
groupFuncs funcs = map (\l -> (fst (head l), map snd l)) (groupBy (\(n,l) (n',l') -> n == n') funcs)

parseFunc :: Parser (String, Line)
parseFunc = message "expected function definition"
          $ pure (,) <*> variable <*> parseLine


parseFull :: Parser x -> String -> Either String x
parseFull p s = case (parse p) s of
    Right (v, []) -> Right v
    Right (_, s) -> Left ("Extra input: " ++ s)
    Left (e, s) -> Left (e ++ " at '" ++ (take 20 s) ++ "'")

{----
(6) Wrap the whole thing in a function
----}

in2out :: String -> String
in2out s = case parseFull parseProg s of
    Right p -> show (eval p [] (EA "main" []))
    Left s -> s

{----
which
  tries to parse its input as a FOUL program, and
    if all the input is successfully consumed,
      evaluates   EA "main" []
        and returns the output (you can use show for that)
    if the input does not parse or is not all consumed
      returns the error message

You'll need to paste in or otherwise link your FOUL interpreter.
----}
