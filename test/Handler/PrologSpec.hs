module Handler.PrologSpec(spec) where

import TestImport hiding(member,cons,insert, run)
import qualified TestImport as I
import Language.Prolog2.IO hiding (clause)
import DBFS hiding (runDB)
import Control.Unification.IntVar
import Control.Unification hiding (getFreeVars, unify)
import qualified Test.Hspec  as H
import Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad.Logger

-- import Language.Prolog2.Trace

-- instance NFData (UTerm T IntVar) where
--   rnf (UTerm (TStruct a ts)) = rnf a `seq` rnf ts

clause lhs rhs = return $ UClause lhs rhs
struct  s x     = UTerm $ TStruct s [x]
struct2 s x y   = UTerm $ TStruct s [x,y]
struct3 s x y z = UTerm $ TStruct s [x,y,z]

not' = struct  "\\+"
(|||)  = struct2 ";"

(|=|)   = struct2 "="
(|\=|)   = struct2 "\\="
(|<|)   = struct2 "<"
(|>|)   = struct2 ">"
(|=<|)  = struct2 "=<"
(|>=|)  = struct2 ">="
(|=:=|) = struct2 "=:="
(|=\=|) = struct2 "=\\="
(|==|)  = struct2 "=="
(|@<|)  = struct2 "@<"
is = struct2 "is"
(|+|) = struct2 "+"
(|-|) = struct2 "-"
(|*|) = struct2 "*"
(|/|) = struct2 "/"

infixr 0 |||
infixr 1 |=| , |\=|, |==|
infixr 2 `is`, |=:=|, |=\=|, |<|, |=<|, |>|,|>=|, |@<|
infixl 3 |+|, |-|
infixl 4 |*|, |/|

num n = atom (T.pack $ show n)
one = num 1
two = num 2
three = num 3
four  = num 4
[a,b,c,d,e] = map atom ["a","b","c","d","e"]


plist :: [Term] -> Term
plist = foldr cons nil


bear     = atom "bear"
elephant = atom "elephant"
cat      = atom "cat"

big   = struct "big"
small = struct "small"
brown = struct "brown"
black = struct "black"
gray  = struct "gray"
dark  = struct "dark"



program1  :: PrologIO Program
program1 = do
  z  <- getFreeVar
  z' <- getFreeVar
  return [ UClause (big bear) []
         , UClause (big elephant) []
         , UClause (small cat) []
         , UClause (brown bear) []
         , UClause (black cat) []
         , UClause (gray elephant) []
         , UClause (dark z)  [black z]
         , UClause (dark z') [brown z']
         ]

goals1 :: PrologIO [Goal]
goals1 = do
  x <- getFreeVar
--  return [ big x, gray x]
  --return [ big x, dark x]
  return [ dark x,big x ]

----------------------------  Program 2 ------------------------------
program2 :: PrologIO Program
program2 = do
  return []
goals2 :: PrologIO [Goal]
goals2 = do
  return [ atom "1" |<|  atom "2" ]

---------------------------- Program 2-1  ----------------------------
program2_1 :: PrologIO Program
program2_1 = do
  return []
goals2_1 :: PrologIO [Goal]
goals2_1 = do
  return [ atom "1" |<|  atom "2"  ||| atom "3" |>| atom "1" ]

------------------------ Program 3 (n queens) ------------------------
queens = struct2 "queens"
queens3 = struct3 "queens3"
attack = struct2 "attack"
attack3 = struct3 "attack3"
selectq = struct3 "selectq"
rangeList2 = struct3 "rangeList2"

program3 :: PrologIO Program
program3 = do
  sequence $ [ do [n,qs,ns] <- getFreeVars 3
                  clause (queens n qs)  [ rangeList2 (num 1) n ns
                                        , queens3 ns nil qs ]
             , do [unplacedQs,safeQs, qs, q, unplacedQs1] <- getFreeVars 5
                  clause (queens3 unplacedQs safeQs qs)  [ selectq q unplacedQs unplacedQs1
                                                         , not' (attack q safeQs)
                                                         , queens3 unplacedQs1 (cons q safeQs) qs ]
             , do qs <- getFreeVar
                  clause (queens3 nil qs qs)            []

             , do [x,xs] <- getFreeVars 2
                  clause (attack x xs)                  [ attack3 x one xs ]

             , do [x,n,y,wildcard] <- getFreeVars 4
                  clause (attack3 x n (cons y wildcard))   [ (x |=:=| (y |+|  n)) ]
--                                                             ||| (x |=:=| (y |-| n)) ]
             , do [x,n,y,wildcard] <- getFreeVars 4
                  clause (attack3 x n (cons y wildcard))   [ (x |=:=| (y |-| n)) ]

             , do [x,n,ys,n1,wildcard] <- getFreeVars 5
                  clause (attack3 x n (cons wildcard ys)) [ n1 `is` (n |+| one)
                                                          , attack3 x n1 ys ]

             , do [x,xs] <- getFreeVars 2
                  clause (selectq x (cons x xs) xs) []
             , do [x,y,ys,zs] <- getFreeVars 4
                  clause (selectq x (cons y ys) (cons y zs)) [ selectq x ys zs ]

             , do [m,n] <- getFreeVars 2
                  clause (rangeList2 m n (cons m nil))  [ m |>=| n ]
             , do [m,n,tail,m1] <- getFreeVars 4
                  clause (rangeList2 m n (cons m tail)) [ m |<| n
                                                        , m1 `is` (m |+| one)
                                                        , rangeList2 m1 n tail ]
             ]


goals3 :: Int ->  PrologIO [Goal]
goals3 n = do [qs,l,x,xs] <- getFreeVars 4
--            return [ not' (attack three (plist [one])) ]
                           -- return [ attack3 one one (plist [two]) ]
--            return [ rangeList2 (num 1) (num 0) qs ]
--            return [ selectq c (plist [a,b,c,d ]) l ]
              return [ queens (num n) qs ]

----------------------------  Program 4 ------------------------------

u = struct2 "="

program4 :: PrologIO Program
program4 = return []
goals4 :: PrologIO [Goal]
goals4   = do
  x <- getFreeVar
--  return [x `u` cons (atom "a") (cons (atom "b") (cons (atom "c") nil)) ]
--  return [x `u` cons (atom "a") nil ]
  return [x `u` cons (atom "a") (cons (atom "b") nil) ]

----------------------------  Program 5 ------------------------------

member = struct2 "member"
program5 :: PrologIO Program
program5 = do
  [x,tail,head] <- getFreeVars 3
  sequence
    [ clause (member x (cons x tail)) []
    , clause (member x (cons head tail)) [member x tail]
    ]
goals5 :: PrologIO [Goal]
goals5 = do
--  return [ member b (cons a (cons b (cons c nil))) ]
--  return [ member d (cons a (cons b (cons c nil))) ]
  x <- getFreeVar
--  return [ member x (cons a (cons b (cons c (cons d (cons e nil))))) ]
  return [ member x (plist [a,b,c,d]) ]
  where [a,b,c,d,e ] = map atom ["a","b","c" , "d" , "e"]
----------------------------  Program 6 ------------------------------

program6 :: PrologIO Program
program6 = program5
goals6 :: PrologIO [Goal]
goals6 = do
  [l,x,y,z] <- getFreeVars 4
  return [ l |=| plist[ x,y,z] ,  member a l , member b l , member c l ]
    where
      [a,b,c] = map atom ["a","b","c"]

---------------------------- - Program 7  ----------------------------
program7 :: PrologIO Program
program7 = program5
goals7 :: PrologIO [Goal]
goals7 = do
  [l,x,y,z,w] <- getFreeVars 5
  return [ l |=| plist [x,y,z,w] ,  member a l , member b l , member c l , member d l]
    where
      [a,b,c,d] = map atom ["a","b","c","d"]

---------------------------- - Program 8  ----------------------------
program8 :: PrologIO Program
program8 = program5
goals8 :: PrologIO [Goal]
goals8 = do
  [dict,sp,en] <- getFreeVars 3
  return [ dict |=| plist [ p one uno , p two dos, p three tres]
         , member (p two sp) dict
         , member (p en tres) dict ]
    where
      p = struct2 "p"
      [one,two,three,uno,dos,tres] = map atom ["one","two","three","uno","dos","tres" ]


---------------------------- - Program 9  ----------------------------
program9 :: PrologIO Program
program9 = do
  [l,l1,l2,l3,x] <- getFreeVars 5
  sequence
    [ clause (conc nil l l) []
    , clause (conc (cons x l1) l2 (cons x l3)) [ conc l1 l2 l3 ]
    ]

conc = struct3 "conc"

goals9 :: PrologIO [Goal]
goals9 = do
  l <- getFreeVar
--  return [ conc (plist [a,b,c]) (plist []) l ]
  return [ conc (plist [a,b,c]) (plist [one,two,three]) l ]
--  return [ conc (plist [a,b]) (plist [one,two]) (plist [a,b,one,two ]) ]
  -- return [ conc (cons a (cons b (cons c nil))) (cons one (cons two (cons three nil))) l ]

----------------------------  Program 10  ----------------------------
program10 :: PrologIO Program
program10 = program9
goals10 :: PrologIO [Goal]
goals10 = do
  [l1,l2] <- getFreeVars 2
  return [ conc l1 l2 (plist [a,b,c]) ]

----------------------------  Program 11  ----------------------------
program11 :: PrologIO Program
program11 = program9
goals11 :: PrologIO [Goal]
goals11 = do
  [before,after,month1,month2,wildcard,wildcard2] <- getFreeVars 6
--  return $ [ conc before (cons may after) (plist (map atom ["jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"] ) ) ]
  return $ [ conc wildcard (cons month1 (cons may (cons month2 wildcard2)))
             (plist (map atom ["jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"] ) ) ]
  where may = atom "may"

----------------------------  Program 12  ----------------------------
program12 :: PrologIO Program
program12 = program9
goals12 :: PrologIO [Goal]
goals12 = do
  [l1,l2,wildcard] <- getFreeVars 3
  return [ l1 `u` plist [a,b,z,z,c,z,z,z,d,e]
         , conc l2 (cons z (cons z (cons z wildcard))) l1 ]
    where
      z = atom "z"
----------------------------  Program 13  ----------------------------
program13 :: PrologIO Program
program13 = do
  [x,l,l1,l2] <- getFreeVars 4
  p <- program9
  q <- sequence [ clause (member1 x l)  [ conc l1 (cons x l2) l ] ]
  return $ p ++ q

goals13 :: PrologIO [Goal]
goals13 = do
  x <- getFreeVar
  return [ member1 x (plist [a,b,c,d,e]) ]

member1 = struct2 "member1"

----------------------------  Program 14  ----------------------------
program14 :: PrologIO Program
program14 = do
  [x,y,z,l,l1] <- getFreeVars 5
  p <- program9
  q <- sequence
      [ clause (delete3 l1 l) [ conc l1 (plist [x,y,z]) l ] ]
  return $ p ++ q

goals14 :: PrologIO [Goal]
goals14 = do
  [l,l1] <- getFreeVars 2
  return [ l |=| plist [a,b,c,d,e] , delete3 l1 l ]


delete3 = struct2 "delete3"

----------------------------  Program 15  ----------------------------
program15 :: PrologIO Program
program15 = do
  [x,l,l1] <- getFreeVars 3
  p <- program9
  q <- sequence
      [ clause (deleteLast1 l1 l) [ conc l1 (plist [x]) l ] ]
  return $ p ++ q

goals15 :: PrologIO [Goal]
goals15 = do
  [l,l1] <- getFreeVars 2
  return [ l |=| plist [a,b,c,d,e] , deleteLast1 l1 l ]

deleteLast1 = struct2 "deleteLast1"

----------------------------  Program 16  ----------------------------
program16 :: PrologIO Program
program16 = do
  [x,y,z,l1,l2] <- getFreeVars 5
  p <- program9
  q <- sequence
      [ clause (deleteLast2 (cons x nil) nil) []
      , clause (deleteLast2 (cons y l1) (cons y l2))
        [ deleteLast2 l1 l2 ]
      ]
  return $ p ++ q

goals16 :: PrologIO [Goal]
goals16 = do
  [l,l1] <- getFreeVars 2
  return [ l |=| plist [a,b,c,d,e] , deleteLast2 l l1 ]

deleteLast2 = struct2 "deleteLast2"

----------------------------  Program 17  ----------------------------
program17 :: PrologIO Program
program17 = do
  [x,y,tail,tail1] <- getFreeVars 4
  q <- sequence
      [ clause (del x (cons x tail) tail) []
      , clause (del x (cons y tail) (cons y tail1))
        [ del x tail tail1 ]
      ]
  return $ q

goals17 :: PrologIO [Goal]
goals17 = do
  [l,l1] <- getFreeVars 2
--  return [ l |=| plist [a,b,c,d,e] , del c l l1 ]
  return [ l |=| plist [a,b,c,d,e] , del one l1 l ]

del = struct3 "del"

----------------------------  Program 18  ----------------------------
program18 :: PrologIO Program
program18 = do
  [s,l,l1,l2,l3] <- getFreeVars 5
  p <- program9
  q <- sequence
      [ clause (sublist s l) [ conc l1 l2 l
                             , conc s  l3 l2 ]
      ]
  return $ p ++ q

goals18 :: PrologIO [Goal]
goals18 = do
  [s] <- getFreeVars 1
  return [ sublist s (plist [a,b,c]) ]

sublist = struct2 "sublist"

----------------------------  Program 19  ----------------------------
program19 :: PrologIO Program
program19 = do
  [x ,list, biggerList] <- getFreeVars 3
  p <- program17
  q <- sequence
      [ clause (insert x list biggerList) [ del x biggerList list ]
      ]
  return $  p ++ q

goals19 :: PrologIO [Goal]
goals19 = do
  [s] <- getFreeVars 1
  return [ insert one (plist [a,b,c]) s ]

insert = struct3 "insert"

----------------------------  Program 20  ----------------------------
program20 :: PrologIO Program
program20 = do
  [x , l, l1, p] <- getFreeVars 4
  prog <- program19
  q <- sequence
      [ clause (permutation nil nil) []
      , clause (permutation (cons x l) p)
        [ permutation l l1
        , insert x l1 p
        ]
      ]
  return $  prog ++ q

goals20 :: PrologIO [Goal]
goals20 = do
  [s] <- getFreeVars 1
  -- return [ permutation (plist [a,b,c]) s ]
  return [ permutation s (plist [a]) ] -- loops

permutation = struct2 "permutation"

----------------------------  Program 21  ----------------------------
program21 :: PrologIO Program
program21 = do
  [x1,x2,l] <- getFreeVars 3
  q <- sequence
      [ clause (evenlength nil) []
      , clause (evenlength (cons x1 (cons x2 l))) [ evenlength l ]
      , clause (oddlength (cons x1 nil)) []
      , clause (oddlength (cons x1 (cons x2 l))) [ oddlength l ]
      ]
  return $  q

goals21 :: PrologIO [Goal]
goals21 = do
--  return [ evenlength (plist [a,b,c,d])  ]
  return [ oddlength (plist [a,b,c,d,e])  ]

evenlength = struct "evenlength"
oddlength = struct "oddlength"

----------------------------  Program 22  ----------------------------
program22 :: PrologIO Program
program22 = do
  [x,l,r,r1,x1,l1,l2] <- getFreeVars 7
  q <- sequence
      [ clause (reverseList nil nil) []
      , clause (reverseList (cons x l) r) [ reverseList l r1 , insertLast x r1 r ]
      , clause (insertLast x nil (cons x nil)) []
      , clause (insertLast x (cons x1 l1) (cons x1 l2)) [ insertLast x l1 l2 ]
      ]
  return $  q

goals22 :: PrologIO [Goal]
goals22 = do
  l <- getFreeVar
  return [ reverseList (plist [a,b,c,d,e]) l  ]

reverseList = struct2 "reverse"
insertLast  = struct3 "insertLast"

----------------------------  Program 23  ----------------------------
program23 :: PrologIO Program
program23 = do
  [x,l,r,r1,x1,l1,l2] <- getFreeVars 7
  p <- program22
  q <- sequence
      [ clause (palindrome nil) []
      , clause (palindrome (cons x nil)) []
      , clause (palindrome (cons x l)) [insertLast x l1 l, palindrome l1]
      ]
  return $  p ++ q

goals23 :: PrologIO [Goal]
goals23 = do
  l <- getFreeVar
  return [ palindrome (plist [a,b,c,b,a]) ]

palindrome = struct "palindrome"

----------------------------  Program 24  ----------------------------
program24 :: PrologIO Program
program24 = do
  [x,l,r,r1,x1,l1,l2] <- getFreeVars 7
  p <- program22
  q <- sequence
      [ clause (shift nil nil) []
      , clause (shift (cons x l) l1) [ insertLast x l l1 ]
      ]
  return $  p ++ q

goals24 :: PrologIO [Goal]
goals24 = do
  [l1,l2] <- getFreeVars 2
  return [ shift (plist [a,b,c,d,e]) l1 , shift l1 l2 ]

shift = struct2 "shift"

----------------------------  Program 25  ----------------------------
program25 :: PrologIO Program
program25 = do
  [x,y,l,m] <- getFreeVars 4
  p <- program22
  q <- sequence
      [ clause (translate nil nil) []
      , clause (translate (cons x l) (cons y m)) [ means x y , translate l m]
      , clause (means (num 0) (atom "zero")) []
      , clause (means (num 1) (atom "one")) []
      , clause (means (num 2) (atom "two")) []
      , clause (means (num 3) (atom "three")) []
      , clause (means (num 4) (atom "four")) []
      , clause (means (num 5) (atom "five")) []
      ]
  return $  p ++ q

goals25 :: PrologIO [Goal]
goals25 = do
  [l1,l2] <- getFreeVars 2
  return [ translate (plist [num 3,num 5,num 1,num 3]) l1 ]

translate = struct2 "translate"
means     = struct2 "means"

----------------------------  Program 26  ----------------------------
program26 :: PrologIO Program
program26 = do
  [x,y,set,sub] <- getFreeVars 4
  p <- program22
  q <- sequence
      [ clause (subset set nil) []
      , clause (subset (cons x set) (cons x sub)) [ subset set sub ]
      , clause (subset (cons x set) (cons y sub)) [ subset set (cons y sub) ]
      ]
  return $  p ++ q

goals26 :: PrologIO [Goal]
goals26 = do
  [s,l2] <- getFreeVars 2
  return [ subset (plist [a,b,c]) s ]

subset = struct2 "subset"

----------------------------  Program 27  ----------------------------
program27 :: PrologIO Program
program27 = do
  [list1,list2,list,x,y,l,m] <- getFreeVars 7
  p <- program9
  q <- sequence
       [ clause (devidelist list list1 list2) [ devide list list1 list2 , about_equal list1 list2 ]

       , clause (devide nil nil nil)                        []
       , clause (devide (cons x list) (cons x list1) list2) [ devide list list1 list2 ]
       , clause (devide (cons x list) list1 (cons x list2)) [ devide list list1 list2 ]

      , clause (about_equal nil nil) []
      , clause (about_equal nil (cons x nil)) []
      , clause (about_equal (cons x nil) nil) []
      , clause (about_equal (cons x l) (cons y m)) [ about_equal l m ]
      ]
  return $  p ++ q

goals27 :: PrologIO [Goal]
goals27 = do
  [s,l2] <- getFreeVars 2
  return [ devidelist (plist [a,b,c,d,e]) (plist [a,b]) l2 ]

devidelist = struct3 "devidelist"
devide = struct3 "devide"
about_equal = struct2 "about_equal"

----------------------------  Program 28  ----------------------------
program28 :: PrologIO Program
program28 = do
  [l1,l2,list,x,y,l,m] <- getFreeVars 7
  p <- program9
  q <- sequence
       [ clause (equal_length nil nil) []
       , clause (equal_length (cons x l1) (cons y l2)) [ equal_length l1 l2]
      ]
  return $  p ++ q

goals28 :: PrologIO [Goal]
goals28 = do
  [s,l2] <- getFreeVars 2
  return [ equal_length (plist [a,b,c,d,e]) (plist [a,b,c,d,e]) ]

equal_length = struct2 "equal_length"

----------------------------  Program 29  ----------------------------
program29 :: PrologIO Program
program29 = do
  [flatlist,tail,l1,l2,x,y] <- getFreeVars 6
  p <- program9
  q <- sequence
       [ clause (flatten nil nil) []
       , clause (flatten (cons x tail) flatlist) [ flatten x l1, flatten tail l2 , conc l1 l2 flatlist ]
       , clause (flatten y (cons y nil)) [ y `u` a ]
      ]
  return $  p ++ q

goals29 :: PrologIO [Goal]
goals29 = do
  [s,l2] <- getFreeVars 2
  return [ flatten (plist [a,a,plist [a,plist [a,a]]]) l2 ]
--  return [ flatten (plist [a,b, plist [c,d], plist [] , plist [plist [plist [e]]], atom "f"]) l2 ]

flatten = struct2 "flatten"

----------------------------  Arithmetic  ----------------------------
----------------------------  Program 30  ----------------------------
program30 :: PrologIO Program
program30 = do
  [m,m1,n,tail,x] <- getFreeVars 5

  q <- sequence
       [
         clause (ge m n)  [ m |>=| n ]
       , clause (rangeList2 m n (cons m nil))  [ m |>=| n ]
       , clause (rangeList2 m n (cons m tail)) [ m |<| n
                                               , m1 `is` (m |+| one)
                                               , rangeList2 m1 n tail ]
       ]
  return $   q

ge = struct2 "ge"

goals30 :: PrologIO [Goal]
goals30 = do
  [l] <- getFreeVars 1
  return [ rangeList2 one three l ]
--  return [ x `is` one |+| two ]
--  return [ ge three two ]

----------------------------  Program 31 Eight Queens  ----------------------------
program31 :: PrologIO Program
program31 = do
  [others,x,y,wildcard,x1,y1,y2,y3,y4,y5,y6,y7,y8] <- getFreeVars 13
  p <- program5
  q <- sequence
       [ clause (solution nil) []
       , clause (solution (cons (x |/| y) others))
         [ solution others
         , member y (plist [one,two,three,four,num 5, num 6])
         , noattack (x |/| y) others
         ]

       , clause (noattack wildcard nil) []
       , clause (noattack (x |/| y) (cons (x1|/|y1) others))
         [ y |=\=| y1
         , y1 |-| y |=\=| x1 |-| x
         , y1 |-| y |=\=| x  |-| x1
         , noattack (x|/|y) others
         ]
       , clause (template (plist [one |/| y1 , two |/| y2 , three |/| y3, four |/| y4 , num 5 |/| y5 , num 6 |/| y6])) []
--       , clause (template4 (plist [one |/| y1, two |/|y2, three |/|y3, four|/|y4 ])) []
       ]
  return $ p ++  q

----------------------------  Program 32 Eight Queens  ----------------------------
program32  :: Int -> PrologIO Program
program32 n = do
  [others,x,y,wildcard,x1,y1,y2,y3,y4,y5,y6,y7,y8] <- getFreeVars 13
  let boardRange = plist $ take n [one,two,three,four,num 5, num 6, num 7, num 8]
  let templateList   = plist $ take n [one |/| y1 , two |/| y2 , three |/| y3, four |/| y4 , num 5 |/| y5 , num 6 |/| y6 , num 7 |/| y7 , num 8 |/| y8 ]

  p <- program5
  q <- sequence
       [ clause (solution nil) []
       , clause (solution (cons (x |/| y) others))
         [ solution others
         , member y boardRange
         , noattack (x |/| y) others
         ]

       , clause (noattack wildcard nil) []
       , clause (noattack (x |/| y) (cons (x1|/|y1) others))
         [ y |=\=| y1
         , y1 |-| y |=\=| x1 |-| x
         , y1 |-| y |=\=| x  |-| x1
         , noattack (x|/|y) others
         ]
       , clause (template templateList) []
       ]
  return $ p ++  q

solution = struct "solution"
noattack = struct2 "noattack"
template = struct "template"

goals32 :: PrologIO [Goal]
goals32 = do
  [s] <- getFreeVars 1
  return [ template s , solution s ]


shouldReturnSolutions :: (PrologIO Program , PrologIO [Goal]) -> Int -> YesodExample site ()
shouldReturnSolutions (prog, goal) n = do
  let monad = do  ps <- prog
                  gs <- goal
                  resolveToTerms () "main" ps gs :: PrologIO [[Term]]

  Right qs <-   liftIO $ runStderrLoggingT $ do
    Right db <- evalPrologT createDB :: LoggingT IO (Either RuntimeError (IODatabase (LoggingT IO)))
    evalRunPrologT monad db
  length qs `shouldBe` n

------------------------------  Specs --------------------------------
spec :: Spec
spec = withApp $ do

   it "tests program 1" $ do
     (program1, goals1) `shouldReturnSolutions` 1
   it "tests program 2" $ do
     (program2, goals2) `shouldReturnSolutions` 1
   it "tests program 3 5" $ do
     (program3, goals3 5) `shouldReturnSolutions` 10
   it "tests program 3 6" $ do
     (program3, goals3 6) `shouldReturnSolutions` 4
   it "tests program 3 7" $ do
     (program3, goals3 7) `shouldReturnSolutions` 40
   it "tests program 4" $ do
     (program4, goals4) `shouldReturnSolutions` 1
   it "tests program 5" $ do
     (program5, goals5) `shouldReturnSolutions` 4
   it "tests program 6" $ do
     (program6, goals6) `shouldReturnSolutions` 6
   it "tests program 7" $ do
     (program7, goals7) `shouldReturnSolutions` 24
   it "tests program 8" $ do
     (program8, goals8) `shouldReturnSolutions` 1
   it "tests program 9" $ do
     (program9, goals9) `shouldReturnSolutions` 1
   it "tests program 10" $ do
     (program10, goals10) `shouldReturnSolutions` 4
   it "tests program 11" $ do
     (program11, goals11) `shouldReturnSolutions` 1
   it "tests program 12" $ do
     (program12, goals12) `shouldReturnSolutions` 1
   it "tests program 13" $ do
     (program13, goals13) `shouldReturnSolutions` 5
   it "tests program 14" $ do
     (program14, goals14) `shouldReturnSolutions` 1
   it "tests program 15" $ do
     (program15, goals15) `shouldReturnSolutions` 1
   it "tests program 16" $ do
     (program16, goals16) `shouldReturnSolutions` 1
   it "tests program 17" $ do
     (program17, goals17) `shouldReturnSolutions` 6
   it "tests program 18" $ do
     (program18, goals18) `shouldReturnSolutions` 10
   it "tests program 19" $ do
     (program19, goals19) `shouldReturnSolutions` 4
   --it "tests program 1" $ do
   --  (program20, goals20) `shouldReturnSolutions` 1 -- loops?
   it "tests program 21" $ do
     (program21, goals21) `shouldReturnSolutions` 1
   it "tests program 22" $ do
     (program22, goals22) `shouldReturnSolutions` 1
   it "tests program 23" $ do
     (program23, goals23) `shouldReturnSolutions` 1
   it "tests program 24" $ do
     (program24, goals24) `shouldReturnSolutions` 1
   it "tests program 25" $ do
     (program25, goals25) `shouldReturnSolutions` 1
   it "tests program 26" $ do
     (program26, goals26) `shouldReturnSolutions` 8
   it "tests program 27" $ do
     (program27, goals27) `shouldReturnSolutions` 1
   it "tests program 28" $ do
     (program28, goals28) `shouldReturnSolutions` 1
   it "tests program 29" $ do
     (program29, goals29) `shouldReturnSolutions` 1
   it "tests program 30" $ do
     (program30, goals30) `shouldReturnSolutions` 1
   --it "tests program 1" $ do
     --(program31, goals31) `shouldReturnSolutions` 1
   it "tests program 32 5" $ do
     (program32 5, goals32) `shouldReturnSolutions` 10
   it "tests program 32 6" $ do
     (program32 6, goals32) `shouldReturnSolutions` 4
   it "tests program 32 7" $ do
     (program32 7, goals32) `shouldReturnSolutions` 40
