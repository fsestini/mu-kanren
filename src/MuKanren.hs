module MuKanren where

import Term
import Data.List (transpose)
import Control.Monad.Trans.Maybe
import Data.Bool

type Elem = Maybe

one :: a -> Elem a
one = Just

delay :: [Elem a] -> [Elem a]
delay = (Nothing :)

type Sub = [(Var, Term)]
type State = (Sub, Int)
type Res = [Elem State]
type Goal = State -> Res

interleave :: [a] -> [a] -> [a]
interleave xs ys = (concat . transpose) [xs, ys]

walk :: Term -> Sub -> Term
walk t@(TVar v) s = maybe t (flip walk s) (lookup v s)
walk t s = t

unify :: Term -> Term -> Sub -> Maybe Sub
unify u v s = aux (walk u s) (walk v s)
  where
    aux :: Term -> Term -> Maybe Sub
    aux (TVar u) (TVar v) | u == v = Just s
    aux (TVar u) v = Just ((u, v) : s)
    aux u (TVar v) = Just ((v, u) : s)
    aux (TPair u v) (TPair u' v') = aux u u' >>= unify v v'
    aux (TData x) (TData y) = x ~= y >>= bool Nothing (Just s)
    aux _ _ = Nothing

(===) :: Term -> Term -> Goal
(u === v) st = maybe [] (\nu -> pure (one (nu, snd st))) (unify u v (fst st))

callFresh :: (Var -> Goal) -> Goal
callFresh f sc = f c (fst sc , c + 1) where c = snd sc

conj, disj :: Goal -> Goal -> Goal
disj g1 g2 sc = interleave (g1 sc) (g2 sc)
conj g1 g2 sc = runMaybeT (MaybeT (g1 sc) >>= MaybeT . g2)

emptyState :: State
emptyState = ([], 0)
