module Programs where

import MuKanren
import Wrappers
import Term

aAndB :: Goal
aAndB = conj (callFresh (\a -> TVar a === num 7))
             (callFresh (\b -> disj (TVar b === num 5) (TVar b === num 6)))

fives :: Term -> Goal
fives x = disj (x === num 5) (delay . fives x)

appendo :: Term -> Term -> Term -> Goal
appendo l s o =
  disj (conj (nil === l) (s === o)) $ runG $ do
    [a,d,r] <- fresh 3
    pure $ conjs [ TPair (TVar a) (TVar d) === l
                 , TPair (TVar a) (TVar r) === o
                 , appendo (TVar d) s (TVar r) ]

callAppendo :: Goal
callAppendo = runG $ do
  [q,l,s,out] <- fresh 4
  pure $ conj (appendo (TVar l) (TVar s) (TVar out))
              (list [TVar l, TVar s, TVar out] === TVar q)

appendoo :: Goal
appendoo = runG $ do
  [q,x,y] <- fresh 3
  pure $ conj (appendo (TVar x) (TVar y) (nums [1,2,3,4,5]))
              (list [TVar x, TVar y] === TVar q)

relo :: Term -> Goal
relo x = runG $ do
  [x1,x2] <- fresh 2
  pure $ conj (x === TPair (TVar x1) (TVar x2))
              (disj (TVar x1 === TVar x2)
                    (delay . relo x))

manyNonAns :: Goal
manyNonAns = callFresh $ \x ->
  disj (relo (TPair (num 5) (num 6))) (TVar x === num 3)
