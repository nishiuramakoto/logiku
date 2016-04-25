module Breadcrumb (breadcrumbWidget) where

import Import hiding(breadcrumb, Form)
import CCGraph


breadcrumbWidget :: CCState -> Widget
breadcrumbWidget st = do
  path' <- handlerToWidget $ spine (ccsCurrentNode st)
  uid   <- handlerToWidget $ getUserAccountId
  let root = getRoot path'

  $(widgetFile "blog_breadcrumb")

  where
    getRoot :: [CCLEdge] -> CCNode
    --getRoot ((root,_,_):_) = 0
    getRoot _ = 0
