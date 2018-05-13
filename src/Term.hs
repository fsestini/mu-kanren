{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GADTs #-}

module Term
  ( Var
  , Atom(..)
  , (~=)
  , Term
  , nil
  , printTerm
  , num
  , nums
  , list
  , pattern TPair
  , pattern TVar
  , pattern TData
  , pattern Term.Nil
  ) where

import Type.Reflection
import Data.SCargot
import Data.SCargot.Repr.Basic
import Data.Text (Text, pack, unpack)

type Var = Int

data Atom where
  AVar :: Var -> Atom
  AData :: (Eq a, Show a, Typeable a) => a -> Atom
deriving instance Show Atom

(~=) :: (Eq a, Typeable a, Typeable b) => a -> b -> Maybe Bool
x ~= y = fmap (\HRefl -> x == y) (eqTypeRep (typeOf x) (typeOf y))

type Term = SExpr Atom

pattern TPair a t = a ::: t
pattern TVar v = A (AVar v)
pattern TData d = A (AData d)
pattern Nil = SNil

nil :: Term
nil = SNil

printAtom :: Atom -> Text
printAtom (AVar v) = pack ("#" ++ show v)
printAtom (AData d) = pack (show d)

printTerm :: Term -> String
printTerm =
  unpack . encodeOne (setIndentStrategy (const Align) $ basicPrint printAtom)

num :: Int -> Term
num = TData

nums :: [Int] -> Term
nums = list . fmap num

list :: [Term] -> Term
list = foldr (:::) SNil
