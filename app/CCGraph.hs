{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
module CCGraph (
  CCK,
  CCP,
  CCNode,
  CCNodeLabel(..),
  CCEdgeLabel(..),
  CCLNode,
  CCLEdge,
  CCGraph,
  CCState(..),
  YesodCC(..),
  Graph.empty,
  run,
  resume,
  answerGet,
--  readCCGraph,
--  takeCCGraph,
--  modifyCCGraph,
  insertCCNode,
  insertCCRoot,
  lookupCCK,
  inquire,
  inquireFinish,
  inquireGet,
  inquireGetUntil,
  inquirePost,
  inquirePostUntil,
  inquirePostButton,
  inquirePostUntilButton,
  generateCCFormGet,
  generateCCFormPost,
  rootPath,
  spine,
  startState
  ) where

import             Import.NoFoundation
import             Text.Blaze (Markup)
import             Control.Monad.CC.CCCxe
import qualified   Data.Text as T
import             Data.Time.LocalTime
import             Data.Graph.Inductive.Graph
import qualified   Data.Graph.Inductive.Graph as Graph
import             Data.Graph.Inductive.PatriciaTree
import             Data.Typeable
import             Form

-- type AppHandler = HandlerT App IO
type CCP                 = PP
type CCK site w          = CCNode  -> CC CCP (HandlerT site IO) w
type CCNode              = Graph.Node
data CCNodeLabel site    = CCNodeLabel { ccTimestamp :: LocalTime , ccK :: Maybe (CCK site Html) }
data CCEdgeLabel         = forall form. (Show form, Typeable form, Eq form)
                           => CCEdgeLabel { ccResponse  :: form }
type CCLNode site        = LNode (CCNodeLabel site)
type CCLEdge             = LEdge (CCEdgeLabel)
type CCGraph site        = Gr (CCNodeLabel site) CCEdgeLabel
type CCHtml site         = CCNode -> CC CCP (HandlerT site IO) Html
data CCState             = forall a. (Show a, Eq a, Typeable a)
                           => CCState { ccsCurrentNode :: CCNode
                                      , ccsCurrentForm :: (FormResult a)}
----------------------  Define the type class ------------------------
class YesodCC site where
  getCCPool :: site -> MVar (CCGraph site)

  newCCPool :: IO (MVar (CCGraph site))
  newCCPool = newMVar Graph.empty

--  takeCCGraph ::  HandlerT site IO (CCGraph site)
  readCCGraph ::  HandlerT site IO (CCGraph site)
  modifyCCGraph :: (CCGraph site -> IO (CCGraph site, b) ) -> HandlerT site IO b

instance Show (CCNodeLabel site) where
  show (CCNodeLabel time (Just _))  = "(" ++ show time ++ ", " ++ "Just <cont>" ++ ")"
  show (CCNodeLabel time (Nothing)) = "(" ++ show time ++ ", " ++ "Nothing" ++ ")"

deriving instance Show CCEdgeLabel
instance Eq   CCEdgeLabel where
  CCEdgeLabel form == CCEdgeLabel form' = cast form == Just form'

deriving instance Typeable CCEdgeLabel

deriving instance Show CCState
instance Eq CCState where
  CCState node a == CCState node' a' = node == node' && cast a == Just a'
deriving instance Typeable CCState


---------------------- - Running continuations  ----------------------

run ::  CC CCP (HandlerT site IO) Html ->  HandlerT site IO Html
run f = do
  $(logInfo) $ T.pack $ "Running a new continuation"

  runCC $ pushPrompt pp f

resume ::  YesodCC site =>  CCNode -> Html -> HandlerT site IO Html
resume node notFoundHtml = do
  $(logInfo) $ T.pack $ "resuming " ++ show node
  mk <- lookupCCK node
  case mk of
    Just k  -> runCC $ k node
    Nothing -> do
      $(logInfo) "Continuation not found"
      return notFoundHtml

-------------- Access to the global continuation store  --------------


insertCCNode ::  YesodCC site => CCK site Html -> HandlerT site IO (CCLNode site)
insertCCNode k = do
  ZonedTime time _tz <- lift $ getZonedTime
  lnode <- modifyCCGraph $ f time
  $(logInfo) $ T.pack $ "insertCCNode: " ++ show lnode
  return lnode
    where
      f time gr = do let [newNode] = newNodes 1 gr
                         newLNode  = (newNode, CCNodeLabel time (Just k))
                     return (insNode newLNode gr, newLNode)

insertCCRoot :: forall site. YesodCC site => HandlerT site IO (CCLNode site)
insertCCRoot = do
  ZonedTime time _tz <- lift $ getZonedTime
  lnode <- modifyCCGraph $ f time :: HandlerT site IO (CCLNode site)
  $(logInfo) $ T.pack $ "insertCCRoot: " ++ show lnode
  return lnode
    where
      f :: LocalTime -> CCGraph site -> IO (CCGraph site, CCLNode site)
      f time gr = do let [newNode] = newNodes 1 gr
                         newLNode  = (newNode, CCNodeLabel time Nothing)
                     return (insNode newLNode gr, newLNode)


insertCCEdge :: (YesodCC site, Eq a,Typeable a, Show a)
                => CCNode -> CCNode -> FormResult a -> HandlerT site IO CCLEdge
insertCCEdge node newNode r = do
  edge <- modifyCCGraph f
  $(logInfo) $ T.pack $ "insertCCEdge: " ++ show edge
  return edge
    where
      f gr = do let newLEdge  = (node, newNode, CCEdgeLabel r)
                return (insEdge newLEdge gr, newLEdge)

insertCCLEdge :: (YesodCC site) => CCLEdge -> HandlerT site IO CCLEdge
insertCCLEdge e = do
  edge <- modifyCCGraph f
  $(logInfo) $ T.pack $ "insertCCLEdge: " ++ show edge
  return edge
    where
      f gr = return (insEdge e gr, e)

delCCLEdge :: YesodCC site => CCLEdge -> HandlerT site IO (CCGraph site)
delCCLEdge edge = do
  gr <- modifyCCGraph f
  $(logInfo) $ T.pack $ "delCCLEdge: " ++ show edge
  return gr
    where
      f gr = do let gr' = delLEdge edge gr
                return (gr',gr')



lookupCCK :: YesodCC site => CCNode -> HandlerT site IO (Maybe (CCK site Html))
lookupCCK node = do
  gr <- readCCGraph
  return $ join $ ccK <$> Graph.lab gr node


---------------------- Continuation primitives  ----------------------
sendk ::  YesodCC site => CCState -> CCHtml site -> CCK site Html -> CC CCP (HandlerT site IO) Html
sendk (CCState node response) html k = do
  (new, _) <- lift $ insertCCNode k
  lift $ insertCCLEdge (node, new, CCEdgeLabel response)

  lift $ $(logInfo) $ T.pack $ "sendk" ++ show node
  html new

inquire :: YesodCC site => CCState -> CCHtml site -> CC CCP (HandlerT site IO) CCNode
inquire st html =  shiftP pp $ sendk st html

inquireFinish ::  YesodCC site => Html -> CC CCP (HandlerT site IO) Html
inquireFinish  html = abortP pp $ return html

inquireGet :: (YesodCC site , Eq a, Show a, Typeable a)
              => CCState
              -> CCHtml site
              -> (Html -> MForm (HandlerT site IO) (FormResult a, t))
              -> CC CCP (HandlerT site IO) CCState
inquireGet st html form = do
  newNode   <- inquire st html
  ((result, _widget), _enctype) <- lift $ runFormGet form
  return (CCState newNode  result)

inquireGetUntil :: (YesodCC site, Eq a,Show a,Typeable a)
                   => CCState
                   -> CCHtml site
                   -> (Html -> MForm (HandlerT site IO) (FormResult a, t))
                   -> CC CCP (HandlerT site IO) CCState
inquireGetUntil st html form = do
  newNode <- inquire st html
  ((result, _widget), _enctype) <- lift $ runFormGet form

  case result of
    FormSuccess _ -> return (CCState newNode result)
    _             -> inquireGetUntil (CCState newNode result) html form


inquirePost :: (YesodCC site,  RenderMessage site FormMessage
               , Eq a, Show a, Typeable a )
               => CCState
               -> CCHtml site
               -> (Html -> MForm (HandlerT site IO) (FormResult a, t))
               -> CC CCP (HandlerT site IO) CCState
inquirePost st html form = do
  newNode <- inquire st html
  ((result, _widget), _enctype) <- lift $ runFormPost form
  return (CCState newNode  result)

inquirePostUntil :: (YesodCC site, RenderMessage site FormMessage
                    , Eq a, Show a, Typeable a)
                    => CCState
                    -> CCHtml site
                    -> (Html -> MForm (HandlerT site IO) (FormResult a, t))
                    -> CC CCP (HandlerT site IO) CCState
inquirePostUntil st html form = do
  newNode <- inquire st  html
  lift $ $(logInfo) $ T.pack $ "inquirePostUntil -> " ++ show newNode
  ((result, _widget), _enctype) <- lift $ runFormPost form

  case result of
    FormSuccess _ -> return (CCState newNode result)
    _             -> inquirePostUntil (CCState newNode result) html form


runFormPostButtons :: (Show b) => [(Text,b)] -> (HandlerT site IO) (Maybe b)
runFormPostButtons [] = return Nothing
runFormPostButtons ((name,value):xs) = do
  p <- lookupPostParam name
  case p of
    Just _  -> return (Just value)
    _       -> runFormPostButtons xs


inquirePostButton :: (YesodCC site, RenderMessage site FormMessage
                     , Show a, Eq a, Typeable a, Show b)
                     => CCState
                     -> CCHtml site
                     -> (Html -> MForm (HandlerT site IO) (FormResult a, t))
                     -> [(Text,b)]
                     -> CC CCP (HandlerT site IO) (CCState, Maybe b)
inquirePostButton st html form buttons = do
  st'  <- inquirePost st html form
  r    <- lift $ runFormPostButtons buttons
  return (st', r)


inquirePostUntilButton :: (YesodCC site,  RenderMessage site FormMessage
                          , Eq a, Typeable a, Show a, Show b)
                          => CCState
                          -> CCHtml site
                          -> (Html -> MForm (HandlerT site IO) (FormResult a, t))
                          -> [(Text,b)]
                          -> CC CCP (HandlerT site IO) (CCState, Maybe b)
inquirePostUntilButton st html form buttons = do
  st'  <- inquirePostUntil st html form
  r    <- lift $ runFormPostButtons buttons

  case r of
    Just _button -> return (st', r)
    Nothing      -> inquirePostUntilButton st' html form buttons


-- answerGet :: (MonadTrans t, Monad (t AppHandler))
--             => (Html -> MForm AppHandler (FormResult a, Widget)) -> t AppHandler a -> t AppHandler a
answerGet :: (MonadTrans t, MonadHandler m, Monad (t m)) =>
             (Markup -> MForm m (FormResult a, t1)) -> t m a -> t m a
answerGet form errorAction = do
  ((result, _widget), _enctype) <- lift $ runFormGet form

  case result of
    FormSuccess r -> return r
    _             -> errorAction



----------------------------  Yesod defs  ----------------------------
generateCCFormGet ::  (RenderMessage (HandlerSite m) FormMessage, MonadHandler m)
                     =>  (Markup -> MForm m (FormResult a, xml)) -> m (xml, Enctype)
generateCCFormGet  form = generateFormGet' form

generateCCFormPost ::  (RenderMessage (HandlerSite m) FormMessage, MonadHandler m)
                     =>  (Markup -> MForm m (FormResult a, xml)) -> m (xml, Enctype)
generateCCFormPost form = generateFormPost form


rootPath :: YesodCC site => CCNode -> HandlerT site IO [CCLEdge]
rootPath node = do
  gr <- readCCGraph
  rootPath' gr node

    where
      rootPath' gr node = do
        let ps = lpre gr node
        case ps of
          []    -> return []
          ((node',la ) :_) -> do
            route' <- rootPath' gr node'
            return $ (node', node, la) : route'


lowerLeftPath :: YesodCC site => CCNode -> HandlerT site IO [CCLEdge]
lowerLeftPath node = do
  gr <- readCCGraph
  lowerLeftPath' gr node

    where
      lowerLeftPath' gr node = do
        let ps = lsuc gr node
        case ps of
          []    -> return []
          ((node',la ) :_) -> do
            route' <- lowerLeftPath' gr node'
            return $ (node, node', la) : route'


spine :: YesodCC site => CCNode -> HandlerT site IO [CCLEdge]
spine node = do
  upper <- rootPath node
  lower <- lowerLeftPath node

  case (upper,lower) of
    ([],_) ->   return $ lower
    (_,[]) ->   return $ reverse upper
    (_,_)  ->   return $ reverse upper ++  lower
  where
    tail (x:xs) = xs
    tail [] = []

startState :: YesodCC site => HandlerT site IO CCState
startState = do   (root,_) <- insertCCRoot
                  return   (CCState root (FormMissing :: FormResult ()))
