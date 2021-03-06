{-# LANGUAGE ViewPatterns, GeneralizedNewtypeDeriving, FlexibleInstances, FlexibleContexts, UndecidableInstances, IncoherentInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

module Prolog.Interpreter
   ( resolve, resolve_
   , MonadTrace(..), withTrace
   , MonadGraphGen(..), runNoGraphT
   )
where
import Import hiding(cons,trace,mapM_,sort)
import qualified Prelude

import Control.Monad.Except
import Control.Monad.CC.CCCxe
import Control.Monad.Reader
import Data.Generics (everywhere, mkT)
-- import Control.Applicative ((<$>),(<*>),(<$),(<*), Applicative(..), Alternative(..))
import Data.List (sort, nub)

import qualified Data.Text as T

import Prolog.Syntax
import Prolog.Unifier
import Prolog.Database
import Prolog.Inquire



builtins :: [Clause]
builtins =
   [ Clause (Struct "="   [var "X", var "X"]) []
   , Clause (Struct "\\=" [var "X", var "X"]) [cut, Struct "false" []]
   , Clause (Struct "\\=" [var "X", var "Y"]) []
   , Clause (Struct "not" [var "A"]) [var "A", cut, Struct "false" []]
   , Clause (Struct "not" [var "A"]) []
   , Clause (Struct "\\+" [var "A"]) [var "A", cut, Struct "false" []]
   , Clause (Struct "\\+" [var "A"]) []
   , Clause (Struct "true" []) []
   , Clause (Struct "," [var "A", var "B"]) [var "A", var "B"]
   , Clause (Struct ";" [var "A", Wildcard]) [var "A"]
   , Clause (Struct ";" [Wildcard, var "B"]) [var "B"]
   , ClauseFn (Struct "is"  [var "L", var "R"]) is
   , ClauseFn (Struct "<"   [var "N", var "M"]) (binaryIntegerPredicate (<))
   , ClauseFn (Struct ">"   [var "N", var "M"]) (binaryIntegerPredicate (>))
   , ClauseFn (Struct "=<"  [var "N", var "M"]) (binaryIntegerPredicate (<=))
   , ClauseFn (Struct ">="  [var "N", var "M"]) (binaryIntegerPredicate (>=))
   , ClauseFn (Struct "=:=" [var "N", var "M"]) (binaryIntegerPredicate (==))
   , ClauseFn (Struct "@<" [var "T1", var "T2"]) (binaryPredicate (<))
   , ClauseFn (Struct "@>" [var "T1", var "T2"]) (binaryPredicate (>))
   , ClauseFn (Struct "@=<"[var "T1", var "T2"]) (binaryPredicate (<=))
   , ClauseFn (Struct "@>="[var "T1", var "T2"]) (binaryPredicate (>=))
   , ClauseFn (Struct "==" [var "T1", var "T2"]) (binaryPredicate (==))
   , ClauseFn (Struct "sort" [var "Input", var "Output"]) (function sort_pl)
   , Clause (Struct "member" [var "X", Struct "." [var "X", Wildcard]]) []
   , Clause (Struct "member" [var "X", Struct "." [Wildcard, var "Xs"]])
                [Struct "member" [var "X", var "Xs"]]
   , ClauseFn (Struct "=.." [var "Term", var "List"]) univ
   , ClauseFn (Struct "atom" [var "T"]) atom
   , ClauseFn (Struct "char_code" [var "Atom", var "Code"]) char_code
   , Clause (Struct "phrase" [var "RuleName", var "InputList"])
               [Struct "phrase" [var "RuleName", var "InputList", Struct "[]" []]]
   , Clause (Struct "phrase" [var "Rule", var "InputList", var "Rest"])
               [ Struct "=.." [var "Rule", var "L"]
               , Struct "append" [var "L", foldr cons nil (arguments [{- already in L -}] (var "InputList") (var "Rest")), var "L1"] -- FIXME This makes assumptions about "arguments"
               , Struct "=.." [var "Goal", var "L1"]
               , var "Goal"
               ]
   , Clause (Struct "append" [Struct "[]" [], var "YS", var "YS"]) []
   , Clause (Struct "append" [Struct "." [var "X", var "XS"], var "YS", Struct "." [var "X", var "XSYS"]]) [Struct "append" [var "XS", var "YS", var "XSYS"]]
   ]
 where
   binaryIntegerPredicate :: (Integer -> Integer -> Bool) -> ([Term] -> [Goal])
   binaryIntegerPredicate p [eval->Just n, eval->Just m] | n `p` m = []
   binaryIntegerPredicate _ _ = [Struct "false" []]

   binaryPredicate :: (Term -> Term -> Bool) -> ([Term] -> [Goal])
   binaryPredicate p [t1, t2] | t1 `p` t2 = []
   binaryPredicate _ _ = [Struct "false" []]

   is [t, eval->Just n] = [Struct "=" [t, Struct (show n) []]]
   is _                 = [Struct "false" []]

   eval (Struct (Prelude.reads->[(n,"")]) []) = return n :: Maybe Integer
   eval (Struct "+" [t1, t2])   = (+) <$> eval t1 <*> eval t2
   eval (Struct "*" [t1, t2])   = (*) <$> eval t1 <*> eval t2
   eval (Struct "-" [t1, t2])   = (-) <$> eval t1 <*> eval t2
   eval (Struct "mod" [t1, t2]) = mod <$> eval t1 <*> eval t2
   eval (Struct "-" [t])        = negate <$> eval t
   eval _                       = mzero

   univ [Struct a ts, list]                        = [Struct "=" [Struct "." [Struct a [], foldr cons nil ts], list]]
   univ [term,        Struct "." [Struct a [], t]] = [Struct "=" [term, Struct a (foldr_pl (:) [] t)]]
   univ _                                          = [Struct "false" []]

   atom [Struct _ []] = []
   atom _             = [Struct "false" []]

   char_code [Struct [c] [], t]               = [Struct "=" [Struct (show (fromEnum c)) [], t]]
   char_code [t, Struct (Prelude.reads->[(n,"")]) []] = [Struct "=" [t, Struct [toEnum n] []]]
   char_code _                                = [Struct "false" []]

   function :: (Term -> Term) -> ([Term] -> [Goal])
   function f [input, output] = [Struct "=" [output, f input]]

   sort_pl = foldr cons nil . nub . sort . foldr_pl (:) []

class Monad m => MonadTrace m where
   trace :: String -> m ()
instance MonadTrace (Trace IO) where
   trace = Trace . Prelude.putStrLn
instance MonadTrace IO where
   trace _ = return ()
instance MonadTrace (Either err) where
   trace _ = return ()
instance (MonadTrace m, MonadTrans t, Monad (t m)) => MonadTrace (t m) where
   trace x = lift (trace x)

instance MonadTrace Handler where
  trace x = $(logInfo) (T.pack x)

instance MonadThrow (CC (PS Html) Handler) where
  throwM e = shiftP ps $ return $ lift $ defaultLayout [whamlet| #{show e} |]


newtype Trace m a = Trace { withTrace :: m a }  deriving (Functor, Applicative, Monad, MonadError e)

trace_ :: forall (m :: * -> *) a. (Show a, MonadTrace m) =>
          String -> a -> m ()

trace_ label x = trace (label++":\t"++show x)


class Monad m => MonadGraphGen m where
   createConnections :: Unifier -> [Goal] -> [Branch] -> m ()
   markSolution :: Unifier -> m ()
   markCutBranches :: Stack -> m ()

instance MonadGraphGen m => MonadGraphGen (ReaderT r m) where
   createConnections x y z = lift (createConnections x y z)
   markSolution = lift . markSolution
   markCutBranches = lift . markCutBranches


newtype NoGraphT m a = NoGraphT {runNoGraphT :: m a} deriving (Monad, Functor, MonadFix, MonadPlus, Applicative, Alternative, MonadThrow)
instance MonadTrans NoGraphT where
   lift = NoGraphT

instance Monad m => MonadGraphGen (NoGraphT m) where
   createConnections _ _ _ = NoGraphT $ return ()
   markSolution      _     = NoGraphT $ return ()
   markCutBranches   _     = NoGraphT $ return ()


data UnknownPredicateException = UnknownPredicateException String
                               deriving (Show, Typeable)

instance Exception UnknownPredicateException

type Stack = [(Unifier, [Goal], [Branch])]
type Branch = (Unifier, [Goal])

--resolve :: (Functor m, MonadTrace m, MonadThrow m) => Program -> [Goal] -> m [Unifier]
resolve :: Program -> [Goal] -> CC (PS Html) Handler [Unifier]
resolve program goals = runNoGraphT (resolve_ program goals)

-- resolve_ :: (Functor m, MonadTrace m, MonadThrow m, MonadGraphGen m) => Program -> [Goal] -> m [Unifier]
resolve_  :: Program -> [Goal] -> NoGraphT (CC (PS Html) Handler) [Unifier]
-- Yield all unifiers that resolve <goal> using the clauses from <program>.
resolve_ program goals = map cleanup <$> runReaderT (resolve' 1 [] goals []) (createDB (builtins ++ program) ["false","fail"])   -- NOTE Is it a good idea to "hardcode" the builtins like this?
  where
      cleanup = filter ((\(VariableName i _) -> i == 0) . fst)

      whenPredicateIsUnknown sig action = asks (hasPredicate sig) >>= flip unless action

      --resolve' :: Int -> Unifier -> [Goal] -> Stack -> m [Unifier]
      resolve' :: Int -> Unifier -> [Goal] -> Stack
               -> ReaderT Database (NoGraphT (CC (PS Html) Handler)) [Unifier]
      resolve' depth usf [] stack = do
         trace "=== yield solution ==="
         trace_ "Depth" depth
         trace_ "Unif." usf

         markSolution usf

         (cleanup usf:) <$> backtrack depth stack
      resolve' depth usf (Cut n:gs) stack = do
         trace "=== resolve' (Cut) ==="
         trace_ "Depth"   depth
         trace_ "Unif."   usf
         trace_ "Goals"   (Cut n:gs)
         Prelude.mapM_ (trace_ "Stack") stack

         createConnections usf (Cut n:gs) [(usf, gs)]

         markCutBranches (take n stack)

         resolve' depth usf gs (drop n stack)

      resolve' depth usf goals'@(Struct "asserta" [fact]:gs) stack = do
         trace "=== resolve' (asserta/1) ==="
         trace_ "Depth"   depth
         trace_ "Unif."   usf
         trace_ "Goals"   goals'
         mapM_ (trace_ "Stack") stack

         createConnections usf goals' [(usf, gs)]

         local (asserta fact) $ resolve' depth usf gs stack

      resolve' depth usf goals'@(Struct "assertz" [fact]:gs) stack = do
         trace "=== resolve' (assertz/1) ==="
         trace_ "Depth"   depth
         trace_ "Unif."   usf
         trace_ "Goals"   goals'
         mapM_ (trace_ "Stack") stack

         createConnections usf goals' [(usf, gs)]

         local (assertz fact) $ resolve' depth usf gs stack

      resolve' depth usf goals'@(Struct "retract" [t]:gs) stack = do
         trace "=== resolve' (retract/1) ==="
         trace_ "Depth"   depth
         trace_ "Unif."   usf
         trace_ "Goals"   goals'
         mapM_ (trace_ "Stack") stack

         createConnections usf goals' [(usf, gs)]

         clauses <- asks (getClauses t)
         case [ t' | Clause t' [] <- clauses, isJust (unify t t') ] of
            []       -> return (fail "retract/1")
            (fact:_) -> local (abolish fact) $ resolve' depth usf gs stack

      resolve' depth usf (InquireBool t v:gs) stack = do
        trace "=== resolve' (inquire_bool/1) ==="
        trace_ "Depth"   depth
        trace_ "Unif."   usf
        trace_ "Goals"   gs
        mapM_ (trace_ "Stack") stack

        (_klabel, form ) <- lift $ lift $ inquirePrologBool  t
        let result = case form of
              FormSuccess (PrologInquireBoolForm True)  -> Struct "true" []
              _                                         -> Struct "false" []

        trace_ "Result" result
        resolve' depth usf (Struct "=" [v, result]:gs) stack

      resolve' depth usf (nextGoal:gs) stack = do
         trace "=== resolve' ==="
         trace_ "Depth"   depth
         trace_ "Unif."   usf
         trace_ "Goals"   (nextGoal:gs)
         mapM_ (trace_ "Stack") stack
         let sig = signature nextGoal
         whenPredicateIsUnknown sig $ do
            throwM $ UnknownPredicateException $ "Unknown predicate: " ++ show sig
         branches <- getBranches

         createConnections usf (nextGoal:gs) branches

         choose depth usf gs branches stack

       where
         getBranches = do
            clauses <- asks (getClauses nextGoal)
            return $ do
               clause <- renameVars clauses
               u <- unify (apply usf nextGoal) (lhs clause)
               let newGoals = rhs clause (map snd u)
               let u' = usf +++ u
               let gs'  = map (apply u') $ newGoals ++ gs
               let gs'' = everywhere (mkT shiftCut) gs'
               return (u', gs'')

         shiftCut (Cut n) = Cut (succ n)
         shiftCut t       = t

         renameVars = everywhere $ mkT $ \(VariableName _ v) -> VariableName depth v

      choose depth _ _  []              stack = backtrack depth stack
      choose depth u gs ((u',gs'):alts) stack = do
         trace "=== choose ==="
         trace_ "Depth"   depth
         trace_ "Unif."   u
         trace_ "Goals"   gs
         mapM_ (trace_ "Alt.") ((u',gs'):alts)
         mapM_ (trace_ "Stack") stack
         resolve' (succ depth) u' gs' ((u,gs,alts) : stack)

      backtrack _     [] = do
         trace "=== give up ==="
         return (fail "Goal cannot be resolved!")
      backtrack depth ((u,gs,alts):stack) = do
         trace "=== backtrack ==="
         choose (pred depth) u gs alts stack
