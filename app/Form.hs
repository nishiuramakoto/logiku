{-# LANGUAGE StandaloneDeriving #-}

module Form
       ( DirectoryForm(..)

       ) where


import Import.NoFoundation


deriving instance Eq  a => Eq  (FormResult a)
deriving instance Ord a => Ord (FormResult a)
deriving instance Typeable a => Typeable (FormResult a)

--------------------------------------------------------------------


data DirectoryForm = DirectoryForm { formDirectoryName        :: Text
                                   , formDirectoryExplanation :: Textarea
                                   , formDirectoryProgram     :: Textarea
                                   } deriving (Eq,Ord,Show,Typeable)
