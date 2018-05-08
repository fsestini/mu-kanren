module Wrappers where

import Data.Foldable
import Term
import MuKanren
import Control.Monad.Cont
import Data.Maybe
import Data.List

ppSub :: Sub -> String
ppSub sc = "[" ++ f sc ++ "]"
  where f = concat . intersperse ", " .
             fmap (\(v,t) -> "(#" ++ show v ++ ", " ++ printTerm t ++ ")")

ppState :: State -> String
ppState (s,c) = "(" ++ ppSub s ++ ", " ++ show c ++ ")"

conjs, disjs :: [Goal] -> Goal
conjs = foldr conj (pure . one)
disjs = foldr disj (pure . one)

conde :: [[Goal]] -> Goal
conde = disjs . fmap conjs

callFresh' :: Cont Goal Var
callFresh' = cont callFresh

fresh :: Int -> Cont Goal [Var]
fresh 0 = pure []
fresh n = (:) <$> callFresh' <*> fresh (n - 1)

fresh' :: Int -> ([Var] -> Goal) -> Goal
fresh' n = runCont (fresh n)

runG :: Cont Goal Goal -> Goal
runG = flip runCont id

printRes :: [State] -> IO ()
printRes = mapM_ (putStrLn . printTerm) . fmap reifyFst

-- truncate at at most m delays
trunc :: Int -> [Elem a] -> [Elem a]
trunc _ [] = []
trunc m (Just x : xs) = Just x : trunc m xs
trunc m (Nothing : xs) = if m == 0 then [] else Nothing : trunc (m - 1) xs

takeLimit :: Int -> Maybe Int -> [Elem a] -> [a]
takeLimit n m = take n . catMaybes . maybe id trunc m

takeS :: Int -> Goal -> IO ()
takeS n g = mapM_ putStrLn . fmap ppState . takeLimit n Nothing $ g emptyState

takeS' :: Int -> Int -> Goal -> IO ()
takeS' n m g = mapM_ putStrLn . fmap ppState . takeLimit n (Just m) $ g emptyState

run :: Int -> Goal -> IO ()
run n g = printRes . takeLimit n Nothing $ g emptyState

runAll :: Goal -> IO ()
runAll g = printRes . catMaybes $ g emptyState

run' :: Int -> Int -> Goal -> IO ()
run' n l g = printRes . takeLimit n (Just l) $ g emptyState

deepWalk :: Term -> Sub -> Term
deepWalk t s = case walk t s of
  TPair t1 t2 -> TPair (deepWalk t1 s) (deepWalk t2 s)
  t -> t

data ReifiedVar = RV Int deriving Eq
instance Show ReifiedVar where show (RV i) = "_." ++ show i

reifyS :: Term -> Sub -> Sub
reifyS t s = case walk t s of
  TVar v -> let n = TData (RV (length s)) in (v, n) : s
  TPair t1 t2 -> reifyS t2 (reifyS t1 s)
  t' -> s

reifyFst :: State -> Term
reifyFst sc = let v = deepWalk (TVar 0) (fst sc) in deepWalk v (reifyS v [])
