module Parser (parseLambda, parseLine) where

import Control.Monad
import Control.Applicative
import Data.Char (isAlpha, isAlphaNum)

import Lambda
import Binding

newtype Parser a = Parser { parse :: String -> Maybe (a, String) }


failParse :: Parser a
failParse = Parser $ \s -> Nothing

aParser :: Parser Char 
aParser = Parser $ \s -> case s of
    [] -> Nothing
    (x:xs) -> if x == 'a' then Just (x, xs) else Nothing


charParser :: Char -> Parser Char
charParser c = Parser $ \s -> case s of
    [] -> Nothing
    (x:xs) -> if x == c then Just (x, xs) else Nothing

predicateParser :: (Char -> Bool) -> Parser Char
predicateParser p = Parser $ \s -> case s of
    [] -> Nothing
    (x:xs) -> if p x then Just (x, xs) else Nothing

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
    empty = failParse
    p1 <|> p2 = Parser $ \s -> case parse p1 s of
        Nothing -> parse p2 s
        Just x -> Just x

variableParser :: Parser String
variableParser = do
    c <- predicateParser isAlpha
    n <- predicateParser isAlphaNum
    return [c, n]

lambdaParser :: Parser Lambda
lambdaParser = do
    charParser 'λ'
    var <- variableParser
    charParser '.'
    e <- lambdaParser
    return $ Abs var e

-- varp :: Parser String
-- varp = do 
--     c <- predicateParser isAlpha
--     n <- predicateParser isAlphaNum
--     return [c, n]

-- plusParser :: Parser a -> Parser [a]
-- plusParser p = do
--     x <- p
--     xs <- starParser p
--     return $ x:xs

-- starParser :: Parser a -> Parser [a]
-- starParser p = (plusParser p) <|> return []

-- varParser :: Parser String
-- varParser = do
--     x <- predicateParser isAlpha
--     xs <- starParser $ predicateParser isAlphaNum
--     return $ x:xs

-- varExprParser :: Parser Lambda
-- varExprParser = Var <$> varParser
-- 2.1. / 3.2.
parseLambda :: String -> Lambda
parseLambda s = undefined


-- whitespaceParser :: Parser ()
-- whitespaceParser = starParser(charParser ' ')

-- valExprParser :: Parser Lambda
-- valExprParser = (Atom . read) <$> plusParser (predicateParser isAlphaNum)

-- someAtom :: Parser Lambda
-- someAtom = valExprParser <|> varExprParser

-- exprPlusParser :: Parser Lambda
-- exprPlusParser = do
--     atom <- someAtom
--     whitespaceParser
--     charParser '+'
--     whitespaceParser
--     e <- exprParser
--     return (atom `plus` e)

-- exprParser :: Parser Lambda
-- exprParser = exprPlusParser <|> someAtom

-- 3.3.
parseLine :: String -> Either String Line
parseLine = undefined
