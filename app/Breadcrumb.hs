module Breadcrumb (breadcrumbWidget) where

import Import hiding(breadcrumb, Form)
import CCGraph

breadcrumbWidget' :: CCState -> Widget
breadcrumbWidget' st@(CCState node _) = do
  [whamlet|breadcrumb|]

breadcrumbWidget :: CCState -> Widget
breadcrumbWidget st@(CCState node _) = do
  path' <- handlerToWidget $ spine node
  uid   <- handlerToWidget $ getUserAccountId
  let root = getRoot path'
  $(widgetFile "blog_breadcrumb")

  where
    getRoot :: [CCLEdge] -> CCNode
    --getRoot ((root,_,_):_) = 0
    getRoot _ = 0
