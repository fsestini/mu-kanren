{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GADTs #-}

module Term where

import Type.Reflection
import Data.SCargot
import Data.SCargot.Repr.Basic
import Data.Text (Text, pack, unpack)

type Var = Int

data Atom where
  AVar :: Var -> Atom
  AData :: (Eq a, Show a, Typeable a) => a -> Atom

instance Show Atom where
  show (AVar v) = "AVar " ++ show v
  show (AData d) = "AData " ++ show d

(~=) :: (Eq a, Typeable a, Typeable b) => a -> b -> Maybe Bool
x ~= y = fmap (\HRefl -> x == y) (eqTypeRep (typeOf x) (typeOf y))

type Term = SExpr Atom

pattern TPair a t = a ::: t
pattern TVar v = A (AVar v)
pattern TData d = A (AData d)

nil :: Term
nil = Nil

printAtom :: Atom -> Text
printAtom (AVar v) = pack ("#" ++ show v)
printAtom (AData d) = pack (show d)

printer :: SExprPrinter Atom Term
printer = setIndentStrategy (const Align) $ basicPrint printAtom

printTerm :: Term -> String
printTerm = unpack . encodeOne printer

num :: Int -> Term
num = TData

nums :: [Int] -> Term
nums = list . fmap num

list :: [Term] -> Term
list = foldr TPair nil
