{-# LANGUAGE StandaloneDeriving #-}

module Form
       ( DirectoryForm(..),
         FileEditRequestJson(..)

       ) where


import Import.NoFoundation
import Data.Aeson.Types


deriving instance Eq  a => Eq  (FormResult a)
deriving instance Ord a => Ord (FormResult a)
deriving instance Typeable a => Typeable (FormResult a)

--------------------------------------------------------------------


data DirectoryForm = DirectoryForm { formDirectoryName        :: Text
                                   , formDirectoryExplanation :: Textarea
                                   , formDirectoryProgram     :: Textarea
                                   } deriving (Eq,Ord,Show,Typeable)


data FileEditRequestJson = FileEditRequestJson
                        { fileEditRequestToken :: Text
                        , fileEditRequestName  :: Text
                        , fileEditRequestExplanation :: Text
                        , fileEditRequestCode  :: Text
                        , fileEditRequestAction :: Text
                        } deriving (Show,Eq)

instance FromJSON FileEditRequestJson where
  parseJSON (Object v) = FileEditRequestJson <$>
                         v .: defaultCsrfParamName <*>
                         v .: "name" <*>
                         v .: "explanation" <*>
                         v .: "code" <*>
                         v .: "action"

  parseJSON invalid    = error $  "type error mismatch:FileEditRequest" ++ show invalid

instance ToJSON FileEditRequestJson where
  toJSON FileEditRequestJson {..} = object
                                [ defaultCsrfParamName .= fileEditRequestToken
                                , "name"   .= fileEditRequestName
                                , "explanation" .= fileEditRequestExplanation
                                , "code"   .= fileEditRequestCode
                                , "action" .= fileEditRequestAction
                                ]
