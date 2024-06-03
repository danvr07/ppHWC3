module Binding where

import Lambda

type Context = [(String, Lambda)]

data Line = Eval Lambda 
          | Binding String Lambda deriving (Eq)

instance Show Line where
    show (Eval l) = show l
    show (Binding s l) = s ++ " = " ++ show l

-- 3.1.
simplifyCtxHelper :: Context -> Lambda -> Either String Lambda
simplifyCtxHelper ctx expr = case expr of
    -- daca expresia este o variabila, o returnam
    Var v       -> Right (Var v)
    -- daca expresia este o aplicatie, aplicam functia pe ambele parti
    App l1 l2 -> do
                    l1' <- simplifyCtxHelper ctx l1
                    l2' <- simplifyCtxHelper ctx l2
                    return (App l1' l2')
    -- daca expresia este o abstractie, aplicam functia pe corp
    Abs v body  -> do
                    body' <- simplifyCtxHelper ctx body
                    return (Abs v body')
    -- daca expresia este un macro, incercam sa-l gasim in context
    Macro m     -> case lookup m ctx of
                      Just l  -> Right l
                      Nothing -> Left "Macrou nedefinit"

-- 3.1. Implementați funcția simplifyCtx
simplifyCtx :: Context -> (Lambda -> Lambda) -> Lambda -> Either String [Lambda]
simplifyCtx ctx step expr = do
    substExpr <- simplifyCtxHelper ctx expr 
    return (simplify step substExpr)

normalCtx :: Context -> Lambda -> Either String [Lambda]
normalCtx ctx = simplifyCtx ctx normalStep

applicativeCtx :: Context -> Lambda -> Either String [Lambda]
applicativeCtx ctx = simplifyCtx ctx applicativeStep
