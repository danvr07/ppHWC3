module Binding where

import Lambda

type Context = [(String, Lambda)]

data Line = Eval Lambda 
          | Binding String Lambda deriving (Eq)

instance Show Line where
    show (Eval l) = show l
    show (Binding s l) = s ++ " = " ++ show l

-- 3.1.
substituteMacros :: Context -> Lambda -> Either String Lambda
substituteMacros ctx expr = case expr of
    Var v       -> Right $ Var v
    App l1 l2   -> do
                    l1' <- substituteMacros ctx l1
                    l2' <- substituteMacros ctx l2
                    return $ App l1' l2'
    Abs v body  -> do
                    body' <- substituteMacros ctx body
                    return $ Abs v body'
    Macro m     -> case lookup m ctx of
                      Just l  -> Right l
                      Nothing -> Left $ "Undefined macro: " ++ m

-- 3.1. Implementați funcția simplifyCtx
simplifyCtx :: Context -> (Lambda -> Lambda) -> Lambda -> Either String [Lambda]
simplifyCtx ctx step expr = do
    substitutedExpr <- substituteMacros ctx expr
    return $ simplify step substitutedExpr


normalCtx :: Context -> Lambda -> Either String [Lambda]
normalCtx ctx = simplifyCtx ctx normalStep

applicativeCtx :: Context -> Lambda -> Either String [Lambda]
applicativeCtx ctx = simplifyCtx ctx applicativeStep
