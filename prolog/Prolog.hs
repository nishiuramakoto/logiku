module Prolog
   ( Term(..), var, cut
   , Clause(..), rhs
   , VariableName(..), Atom, Unifier, Substitution, Program, Goal
   , unify, unify_with_occurs_check
   , apply
   , MonadTrace(..)
   , withTrace
   , MonadGraphGen(..)
   , runNoGraphT
   , resolve, resolve_
   , (+++)
   , consult, parseQuery, consultString
   , program, whitespace, comment, clause, terms, term, bottom, vname
   )
where

import Prolog.Syntax
import Prolog.Parser
import Prolog.Unifier
import Prolog.Interpreter
