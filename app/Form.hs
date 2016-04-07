{-# LANGUAGE StandaloneDeriving #-}

module Form
       ( Form(..)
       , FormEdge(..)
       , PrologInquireBoolForm(..)
       , UserForm(..)
       , BlogForm(..)
       ) where


import Import.NoFoundation

data Form = FormPrologInquireBoolForm (FormResult PrologInquireBoolForm)
          | FormUserForm              (FormResult UserForm)
          | FormBlogForm              (FormResult BlogForm)
          | FormEmptyForm             (FormResult ())
            deriving (Eq,Ord,Show)


class FormEdge a where
  formInj :: FormResult a -> Form

deriving instance Eq  a => Eq  (FormResult a)
deriving instance Ord a => Ord (FormResult a)

--------------------------------------------------------------------

data PrologInquireBoolForm = PrologInquireBoolForm Bool
                             deriving (Eq,Ord,Show)


instance FormEdge PrologInquireBoolForm where
  formInj = FormPrologInquireBoolForm


data UserForm = UserForm
                { userformName     :: Text
                , userformPassword :: Text
                } deriving (Eq,Ord,Show)


instance FormEdge UserForm where
  formInj = FormUserForm


data BlogForm = BlogForm { blogformTitle :: Text
                         , blogformBody  :: Textarea
                         } deriving (Eq,Ord,Show)

instance FormEdge BlogForm where
  formInj = FormBlogForm


instance FormEdge () where
  formInj = FormEmptyForm
