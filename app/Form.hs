module Form
       ( Form(..)
       , PrologInquireBoolForm(..)
       ) where

import Import.NoFoundation

data Form = BoolForm PrologInquireBoolForm
          deriving (Eq,Ord,Show,Read)

data PrologInquireBoolForm = PrologInquireBoolForm Bool
                             deriving (Eq,Ord,Show,Read)
