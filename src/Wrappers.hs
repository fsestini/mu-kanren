module Wrappers where

import Data.Foldable
import Term
import DStream
import MuKanren
import Control.Monad.Cont
import Data.List

ppSub :: Sub -> String
ppSub sc = "[" ++ f sc ++ "]"
  where f = concat . intersperse ", " .
             fmap (\(v,t) -> "(#" ++ show v ++ ", " ++ printTerm t ++ ")")

ppState :: State -> String
ppState (s,c) = "(" ++ ppSub s ++ ", " ++ show c ++ ")"

conjs, disjs :: [Goal] -> Goal
conjs = foldr conj pure
disjs = foldr disj pure

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

takeS :: Int -> Goal -> IO ()
takeS n g = mapM_ putStrLn . fmap ppState . take n . toList $ g emptyState

takeS' :: Int -> Int -> Goal -> IO ()
takeS' n l g =
  mapM_ putStrLn . fmap ppState . toList . takeLimit n l $ g emptyState

run :: Int -> Goal -> IO ()
run n g = printRes . take n . toList $ g emptyState

runAll :: Goal -> IO ()
runAll g = printRes . toList $ g emptyState

run' :: Int -> Int -> Goal -> IO ()
run' n l g = printRes . toList . takeLimit n l $ g emptyState

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
