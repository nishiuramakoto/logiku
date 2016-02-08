{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

module Prolog.Database
   ( createDB
   , hasPredicate
   , getClauses
   , asserta
   , assertz
   , abolish
   , Signature(), signature
   , Database(..)
   )
where
import Import
-- import Data.MonoTraversable(MonoFoldable(..))
-- import Data.Map (Map)
import qualified Data.Map as Map

import Prolog.Syntax


data Signature = Signature Atom Int deriving (Ord, Eq)
instance Show Signature where
   show (Signature name arity) = name ++ "/" ++ show arity

signature :: Term -> Signature
signature (Struct name ts) = Signature name (length ts)
signature (InquireBool _t) = Signature "inquire_bool" 1
signature _ = Signature "no signature" 0

newtype Database = DB (Map Signature [Clause])

hasPredicate :: Signature -> Database -> Bool
hasPredicate sig (DB index') = Map.member sig index'

createDB :: forall c. (MonoFoldable c, Element c ~ Clause) =>
            c -> [Atom] -> Database
createDB clauses emptyPredicates = DB $
   foldr (\clause -> Map.insertWith' (++) (signature (lhs clause)) [clause])
         (Map.fromList [ (signature (Struct name []), []) | name <- emptyPredicates ])
         clauses

getClauses :: Term -> Database -> [Clause]
getClauses term (DB index') = maybe [] id $ Map.lookup (signature term) index'

asserta :: Term -> Database -> Database
asserta fact (DB index') = DB $ Map.insertWith (++)        (signature fact) [Clause fact []] index'

assertz :: Term -> Database -> Database
assertz fact (DB index') = DB $ Map.insertWith (flip (++)) (signature fact) [Clause fact []] index'

abolish :: Term -> Database -> Database
abolish fact (DB index') = DB $ Map.adjust deleteFact (signature fact) index'
   where deleteFact (Clause t []:cs) | t == fact = cs
         deleteFact (_          :cs)             = cs
         deleteFact []                           = []
