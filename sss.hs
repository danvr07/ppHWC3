module Parser (parseLambda, parseLine) where

import Control.Monad
import Control.Applicative
import Data.Char (isAlpha, isSpace)

import Lambda
import Binding

-- Definim un tip de date pentru parser
newtype Parser a = Parser { parse :: String -> Maybe (a, String) }

-- Instanțe pentru diferite tipuri de clase
instance Monad Parser where
    return v = Parser $ \s -> Just (v, s)
    mp >>= f = Parser $ \s -> case parse mp s of
        Nothing -> Nothing
        Just (v, s') -> parse (f v) s'

instance Functor Parser where
    fmap f p =
        do v <- p
           return $ f v

instance Applicative Parser where
    af <*> av =
        do f <- af
           v <- av
           return $ f v
    pure = return

instance Alternative Parser where
    empty = Parser $ \_ -> Nothing
    p1 <|> p2 = Parser $ \s -> case parse p1 s of
        Nothing -> parse p2 s
        justResult -> justResult


-- Parser pentru un singur caracter
charParser :: Char -> Parser Char
charParser c =
  Parser
    ( \str -> case str of
        (x : xs) -> if x == c then Just (x, xs) else Nothing
        _ -> Nothing
    )

-- Parser pentru un caracter care îndeplinește o anumită proprietate
predicateParser :: (Char -> Bool) -> Parser Char
predicateParser p =
  Parser
    ( \str -> case str of
        (x : xs) -> if p x then Just (x, xs) else Nothing
        _ -> Nothing
    )


-- Parser pentru variabile
parse_variable :: Parser Lambda
parse_variable = do
  _ <- many (predicateParser isSpace) -- Se ignoră spațiile albe
  c <- predicateParser isAlpha
  return (Var [c])

-- Parser pentru funcții
parse_function :: Parser Lambda
parse_function = do
  _ <- many (predicateParser isSpace) -- Se ignoră spațiile albe
  charParser '\\'
  _ <- many (predicateParser isSpace) -- Se ignoră spațiile albe
  c <- predicateParser isAlpha
  _ <- many (predicateParser isSpace) -- Se ignoră spațiile albe
  charParser '.'
  _ <- many (predicateParser isSpace) -- Se ignoră spațiile albe
  e <- expr_atom
  return (Abs [c] e)

-- Parser pentru aplicații
parse_application :: Parser Lambda
parse_application = do
  e1 <- expr_atom
  e2s <- many (predicateParser isSpace >> expr_atom) -- Se ignoră spațiile albe între expresii
  return (foldl App e1 e2s)

-- Parser pentru paranteze
parse_application_paranthesis :: Parser Lambda
parse_application_paranthesis = do
  charParser '('
  e <- parse_application
  charParser ')'
  return e

-- Parser pentru macro-uri
parse_macro :: Parser Lambda
parse_macro = do
  charParser '$'
  str <- some (predicateParser isAlpha)
  return (Macro str)

-- Expresie atomică (folosită în compunerea altor expresii)
expr_atom :: Parser Lambda
expr_atom = parse_application_paranthesis <|> parse_function <|> parse_macro <|> parse_variable

-- Parserul principal care combină toate tipurile de expresii
expr :: Parser Lambda
expr = parse_application <|> parse_macro <|> parse_variable <|> parse_function

-- Funcția care primește un șir de caractere și îl parsează într-o expresie lambda
parseLambda :: String ->Lambda
parseLambda s = case parse expr s of
  Just (e, "") -> e
  _ -> error "Parse error"


parseLine :: String -> Either String Line
parseLine = undefined

