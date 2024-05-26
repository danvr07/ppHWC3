module Lambda where

import Data.List (nub, (\\))
import Control.Monad (replicateM)
import qualified Data.Set as Set

data Lambda = Var String
            | App Lambda Lambda
            | Abs String Lambda
            | Macro String

instance Show Lambda where
    show (Var x) = x
    show (App e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
    show (Abs x e) = "λ" ++ x ++ "." ++ show e
    show (Macro x) = x

instance Eq Lambda where
    e1 == e2 = eq e1 e2 ([],[],[])
      where
        eq (Var x) (Var y) (env,xb,yb) = elem (x,y) env || (not $ elem x xb || elem y yb)
        eq (App e1 e2) (App f1 f2) env = eq e1 f1 env && eq e2 f2 env
        eq (Abs x e) (Abs y f) (env,xb,yb) = eq e f ((x,y):env,x:xb,y:yb)
        eq (Macro x) (Macro y) _ = x == y
        eq _ _ _ = False

-- 1.1.
vars :: Lambda -> [String]
vars (Var x) = [x]
vars (App e1 e2) = nub $ vars e1 ++ vars e2  -- num elimina duplicatele
vars (Abs x e) = nub $ x : vars e
vars (Macro _) = []


-- 1.2.
freeVars :: Lambda -> [String]
freeVars (Var x) = [x]
freeVars (App e1 e2) = nub $ freeVars e1 ++ freeVars e2
freeVars (Abs x e) = freeVars e \\ [x]
freeVars (Macro _) = []

-- 1.3.
newVar :: [String] -> String
newVar xs = head $ filter (`notElem` xs) candidates
  where
    candidates = [1..] >>= flip replicateM ['a'..'z']
    
-- 1.4.
isNormalForm :: Lambda -> Bool
isNormalForm (Var _) = True
isNormalForm (Abs _ e) = isNormalForm(e)
isNormalForm (App (Abs _ _) _) = False
isNormalForm (App e1 e2) = isNormalForm e1 && isNormalForm e2
isNormalForm (Macro _) = True  -- Assuming Macros are treated as variables for simplicity.


-- 1.5.
reduce :: String -> Lambda -> Lambda -> Lambda
reduce x e1 e2 = subst x e2 e1
  where
    subst :: String -> Lambda -> Lambda -> Lambda
    subst x e (Var y)
      | x == y    = e
      | otherwise = Var y
    subst x e (App e1 e2) = App (subst x e e1) (subst x e e2)
    subst x e (Abs y e1)
      | x == y    = Abs y e1
      | y `elem` freeVars e = 
          let z = newVar (vars e1 ++ vars e ++ [x])
              e1' = subst y (Var z) e1
          in Abs z (subst x e e1')
      | otherwise = Abs y (subst x e e1)
    subst _ _ (Macro m) = Macro m


-- 1.6.
-- normalStep :: Lambda -> Lambda
-- normalStep (App (Abs x e) e2) = reduce e x e2
-- normalStep (App e1 e2) = case normalStep e1 of
--                            e1'@(Abs _ _) -> App e1' e2
--                            e1' -> App e1' (normalStep e2)
-- normalStep e = e

normalStep :: Lambda -> Lambda
normalStep (App (Abs x e1) e2) = reduce x e1 e2
normalStep (App e1 e2)
  | e1 == normalStep e1 = App e1 (normalStep e2)
  | otherwise = App (normalStep e1) e2
normalStep (Abs x e) = Abs x (normalStep e)
normalStep e = e



-- 1.7.
applicativeStep :: Lambda -> Lambda
applicativeStep (App (Abs x e1)(Var e2)) = reduce x e1 (Var e2)
applicativeStep (App (Abs x e1)(Abs y e2)) = reduce x e1 (Abs y e2)
applicativeStep (App (Abs x e1) e2) = App (Abs x e1) (applicativeStep e2)
applicativeStep (App e1 e2)
  | e1 == applicativeStep e1 = App e1 (normalStep e2)
  | otherwise = App (normalStep e1) e2
applicativeStep (Abs x e) = Abs x (applicativeStep e)
applicativeStep e = e

-- 1.8.
simplify :: (Lambda -> Lambda) -> Lambda -> [Lambda]
simplify step e = e : if e == e' then [] else simplify step e'
  where
    e' = step e

normal :: Lambda -> [Lambda]
normal = simplify normalStep

applicative :: Lambda -> [Lambda]
applicative = simplify applicativeStep
