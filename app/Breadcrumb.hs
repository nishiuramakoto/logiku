module Breadcrumb (breadcrumbWidget) where

import Import hiding(breadcrumb, Form)
import CCGraph


breadcrumbWidget :: CCState -> Widget
breadcrumbWidget st = do
  path' <- handlerToWidget $ spine (ccsCurrentNode st)
  uid   <- handlerToWidget $ getUserAccountId

  $(widgetFile "blog_breadcrumb")
