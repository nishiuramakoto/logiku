module ShowText where

import Import.NoFoundation
import Data.Text(Text)
import qualified Data.Text as T


class ShowText a where
  showT :: a -> Text

instance ShowText Text   where showT = id
instance ShowText Int    where showT = T.pack . show

instance (ShowText a, ShowText b)
         => ShowText (a,b) where
  showT (a,b) = T.concat [ "(" , showT a, ",", showT b, ")"]


instance (ShowText a, ShowText b, ShowText c)
         => ShowText (a,b,c) where
  showT (a,b,c) = T.concat [ "(" , showT a, ",", showT b, "," , showT c , ")"]


instance (ShowText a, ShowText b, ShowText c, ShowText d)
         => ShowText (a,b,c,d) where
  showT (a,b,c,d) = T.concat [ "(" , showT a, ",", showT b, "," , showT c, "," , showT d , ")"]


instance (ShowText a, ShowText b, ShowText c, ShowText d, ShowText e)
         => ShowText (a,b,c,d,e) where
  showT (a,b,c,d,e) = T.concat [ "(" , showT a, ",", showT b, "," , showT c, "," , showT d, ",", showT e, ")"]
