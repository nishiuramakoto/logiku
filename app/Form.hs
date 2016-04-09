{-# LANGUAGE StandaloneDeriving #-}

module Form
       ( Form(..)
       , FormEdge(..)
       , PrologInquireBoolForm(..)
       , UserForm(..)
       , BlogForm(..)
       , DirectoryForm(..)
       , FileForm(..)
       , PrologRunnerForm(..)
       , DummyForm(..)
       ) where


import Import.NoFoundation

data Form = FormPrologInquireBoolForm (FormResult PrologInquireBoolForm)
          | FormUserForm              (FormResult UserForm)
          | FormBlogForm              (FormResult BlogForm)
          | FormDirectoryForm         (FormResult DirectoryForm)
          | FormFileForm              (FormResult FileForm)
          | FormPrologRunnerForm      (FormResult PrologRunnerForm)
          | FormDummyForm             (FormResult DummyForm)
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


data DirectoryForm = DirectoryForm { directoryName        :: Text
                                   , directoryExplanation :: Textarea
                                   , directoryProgram     :: Textarea
                                   } deriving (Eq,Ord,Show)
instance FormEdge DirectoryForm where
  formInj = FormDirectoryForm

data DummyForm = DummyForm Bool deriving (Eq,Ord,Show)
instance FormEdge DummyForm where
  formInj = FormDummyForm


data FileForm = FileForm { goalName   ::  Text
                         , goalExpl   ::  Textarea
                         , goalCode   ::  Textarea
                         } deriving (Eq,Ord,Show)
instance FormEdge FileForm where
  formInj = FormFileForm

data PrologRunnerForm = PrologRunnerForm deriving (Eq,Ord,Show)
instance FormEdge PrologRunnerForm where
  formInj = FormPrologRunnerForm

instance FormEdge () where
  formInj = FormEmptyForm
