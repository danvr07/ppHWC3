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
vars (App e1 e2) = nub $ vars e1 ++ vars e2
vars (Abs x e) = nub $ x : vars e 
vars (Macro _) = []

-- 1.2.
freeVars :: Lambda -> [String]
freeVars (Var x) = [x]
freeVars (App e1 e2) = nub $ freeVars e1 ++ freeVars e2
freeVars (Abs x e) = nub $ filter(/= x) (freeVars e)
freeVars (Macro _) = []

-- 1.3.`
newVar :: [String] -> String
newVar xs = head $ filter (`notElem` xs) candidates
  where
    candidates = [1..] >>= flip replicateM ['a'..'z']
-- 1.4.
isNormalForm :: Lambda -> Bool
isNormalForm (Var _) = True
isNormalForm (App (Abs _ _)_) = False
isNormalForm (App e1 e2) = isNormalForm e1 && isNormalForm e2
isNormalForm (Abs _ e) = isNormalForm(e)
isNormalForm (Macro _) = False

-- 1.5.
reduce :: String -> Lambda -> Lambda -> Lambda
reduce x e1 e2 = reduceHelper x e2 e1
  where
    reduceHelper :: String -> Lambda -> Lambda -> Lambda
    --cazul in care expresia este o variabila
    reduceHelper x e (Var y)
      | x == y    = e
      | otherwise = Var y
    --cazul in care expresia este o aplicatie
    reduceHelper x e (App e1 e2) = App (reduceHelper x e e1) (reduceHelper x e e2)
    --cazul in care expresia este o abstractie
    reduceHelper x e (Abs y e1)
      | x == y    = Abs y e1
      | y `elem` freeVars e = 
          let z = newVar (vars e1 ++ vars e ++ [x])
              e1' = reduceHelper y (Var z) e1
          in Abs z (reduceHelper x e e1')
      | otherwise = Abs y (reduceHelper x e e1)
    --cazul in care expresia este un macro
    reduceHelper _ _ (Macro m) = Macro m -- nu se poate reduce un macro

normalStep :: Lambda -> Lambda
normalStep (App (Abs x e) e2) = reduce x e e2
normalStep (App e1 e2)
  | isNormalForm e1 = App e1 (normalStep e2)
  | otherwise = App (normalStep e1) e2
normalStep (Abs x e) = Abs x (normalStep e)
normalStep e = e

-- 1.7.
applicativeStep :: Lambda -> Lambda
applicativeStep (App (Abs x e) (Var e2)) = reduce x e (Var e2)
applicativeStep (App (Abs x e1)(Abs y e2)) = reduce x e1 (Abs y e2)
applicativeStep (App (Abs x e1) e2) = App (Abs x e1) (applicativeStep e2)
applicativeStep (App e1 e2)
  | isNormalForm e1 = App e1 (applicativeStep e2)
  | otherwise = App (applicativeStep e1) e2
applicativeStep (Abs x e) = Abs x (applicativeStep e)
applicativeStep e = e

-- 1.8.
simplify :: (Lambda -> Lambda) -> Lambda -> [Lambda]
simplify step e = e : case trasnformedE == e of
    True  -> []
    False -> simplify step trasnformedE
  where
   trasnformedE = step e

normal :: Lambda -> [Lambda]
normal = simplify normalStep

applicative :: Lambda -> [Lambda]
applicative = simplify applicativeStep
