module Parser (parseLambda, parseLine) where

import Control.Monad
import Control.Applicative
import Data.Char (isAlpha, isAlphaNum , isUpper, isDigit)

import Lambda
import Binding

newtype Parser a = Parser { parse :: String -> Maybe (a, String) }

-- Instanța Monad pentru Parser
instance Monad Parser where
    return v = Parser $ \s -> Just (v, s)
    mp >>= f = Parser $ \s -> case parse mp s of
                                Nothing -> Nothing
                                Just (v, rest) -> parse (f v) rest 

-- Instanța Applicative pentru Parser
instance Applicative Parser where
  af <*> mp = do 
    f <- af
    v <- mp
    return $ f v
  pure = return

-- Instanța Functor pentru Parser
instance Functor Parser where
    fmap f mp = do
        x <- mp
        return $ f x

-- Instanța Alternative pentru Parser
instance Alternative Parser where
    empty = Parser $ \_ -> Nothing
    p1 <|> p2 = Parser $ \s -> case parse p1 s of
                                Nothing -> parse p2 s 
                                res -> res 

failParser :: Parser a
failParser = Parser $ \s -> Nothing

plusParser :: Parser a -> Parser[a]
plusParser p = do
    x <- p
    xs <- starParser p
    return (x:xs)

starParser :: Parser a -> Parser[a]
starParser p = plusParser p <|> return []

charParser :: Char -> Parser Char
charParser c = Parser $ \s -> 
    case s of 
        [] -> Nothing
        (x:xs) -> if x == c then Just (x, xs) else Nothing

predicateParser :: (Char -> Bool) -> Parser Char
predicateParser p = Parser $ \s -> 
    case s of 
        [] -> Nothing
        (x:xs) -> if p x then Just (x, xs) else Nothing


-- Parsere pentru spații
whitespaceParser :: Parser String
whitespaceParser = starParser (charParser ' ')

-- Parsere pentru variabile și identificatori
varParser :: Parser String
varParser = do 
    x <- predicateParser isAlpha
    xs <- many (predicateParser isAlphaNum)
    return (x:xs)

macroParser :: Parser Lambda
macroParser = do
    xs <- some (predicateParser (\c -> isUpper c || isDigit c))
    return (Macro xs)

-- Parsere pentru lambda termeni
varExprParser :: Parser Lambda
varExprParser = Var <$> varParser


--Parser pentru lambda abstracti
absParser :: Parser Lambda
absParser = do
    charParser '\\' <|> charParser 'λ'
    whitespaceParser
    var <- varParser
    whitespaceParser
    charParser '.'
    whitespaceParser
    body <- exprParser
    return (Abs var body)


-- Parser pentru aplicări de lambda termeni
appParser :: Parser Lambda
appParser = do
    charParser '('
    whitespaceParser
    e1 <- exprParser
    whitespaceParser
    e2 <- exprParser
    whitespaceParser
    charParser ')'
    return (App e1 e2)

-- Parsere pentru expresii complete
exprParser :: Parser Lambda
exprParser = macroParser <|> varExprParser <|> absParser <|> appParser

-- 2.1. / 3.2.parseLambda :: String -> Lambda
parseLambda input = case parse exprParser input of
    Nothing -> error "Intrare nevalidă"
    Just (result, _) -> result

-- 3.3.
bindingParser :: Parser Line
bindingParser = do
    name <- varParser
    whitespaceParser
    charParser '='
    whitespaceParser
    expr <- exprParser
    return (Binding name expr)

evalParser :: Parser Line
evalParser = Eval <$> exprParser

lineParser :: Parser Line
lineParser = bindingParser <|> evalParser

parseLine :: String -> Either String Line
parseLine input = case parse lineParser input of
    Nothing -> Left "Intrare nevalidă"
    Just (result, remaining) -> 
        if null remaining 
            then Right result 
            else Left "Intrare neconsumată complet"
