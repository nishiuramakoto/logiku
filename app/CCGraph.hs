{-# LANGUAGE ScopedTypeVariables #-}
module CCGraph (
  CCK,
  CCP,
  CCNode,
  CCNodeLabel(..),
  CCEdgeLabel(..),
  CCLNode,
  CCLEdge,
  CCGraph,
  YesodCC(..),
  run,
  resume,
  answerGet,
  readCCGraph,
  takeCCGraph,
  modifyCCGraph,
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
  routePath
  ) where

import             Import.NoFoundation
import             Text.Blaze (Markup)
import             Control.Monad.CC.CCCxe
import             Control.Exception.Base
import qualified   Data.Text as T
import             Data.Time.LocalTime
import             Data.Graph.Inductive.Graph
import qualified   Data.Graph.Inductive.Graph as Graph
import             Data.Graph.Inductive.PatriciaTree
import             Form

-- type AppHandler = HandlerT App IO
type CCP                 = P2 CCNode Html
type CCK site w          = CCNode  -> CC CCP (HandlerT site IO) w
type CCNode              = Graph.Node
data CCNodeLabel site    = CCNodeLabel { ccTimestamp :: LocalTime , ccK :: Maybe (CCK site Html) }
data CCEdgeLabel         = CCEdgeLabel { ccResponse  :: Form }  deriving (Show,Ord,Eq)
type CCLNode site        = LNode (CCNodeLabel site)
type CCLEdge             = LEdge (CCEdgeLabel)
type CCGraph site        = Gr (CCNodeLabel site) CCEdgeLabel
type CCHtml site         = CCNode -> CC CCP (HandlerT site IO) Html

----------------------  Define the type class ------------------------
class YesodCC site where
  getCCPool :: site -> MVar (CCGraph site)

  newCCPool :: IO (MVar (CCGraph site))
  newCCPool = newMVar Graph.empty

instance Show (CCNodeLabel site) where
  show (CCNodeLabel time (Just _))  = "(" ++ show time ++ ", " ++ "Just <cont>" ++ ")"
  show (CCNodeLabel time (Nothing)) = "(" ++ show time ++ ", " ++ "Nothing" ++ ")"

---------------------- - Running continuations  ----------------------

run ::  CC CCP (HandlerT site IO) Html ->  HandlerT site IO Html
run f = do
  $(logInfo) $ T.pack $ "Running a new continuation"

  runCC $ pushPrompt p2R f

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

takeCCGraph :: YesodCC site => HandlerT site IO  (CCGraph site)
takeCCGraph = do
  yesod <- getYesod
  liftIO $ takeMVar $ getCCPool yesod

readCCGraph :: YesodCC site => HandlerT site IO  (CCGraph site)
readCCGraph = do
  yesod <- getYesod
  let mv = getCCPool yesod
  liftIO $ readMVar mv

modifyCCGraph :: YesodCC site => (CCGraph site -> IO (CCGraph site, b) ) -> HandlerT site IO b
modifyCCGraph f = do
  yesod <- getYesod
  let mv = getCCPool yesod
  liftIO $ modifyMVar mv f'
    where f' gr = do (gr',x) <- f gr
                     gr'' <- evaluate gr'
                     return (gr'',x)

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


insertCCEdge :: (YesodCC site, FormEdge a) => CCNode -> CCNode -> FormResult a -> HandlerT site IO CCLEdge
insertCCEdge node newNode r = do
  edge <- modifyCCGraph f
  $(logInfo) $ T.pack $ "insertCCEdge: " ++ show edge
  return edge
    where
      f gr = do let newLEdge  = (node, newNode, CCEdgeLabel (formInj r))
                return (insEdge newLEdge gr, newLEdge)

lookupCCK :: YesodCC site => CCNode -> HandlerT site IO (Maybe (CCK site Html))
lookupCCK node = do
  gr <- readCCGraph
  return $ join $ ccK <$> Graph.lab gr node



---------------------- Continuation primitives  ----------------------
sendk ::  YesodCC site => CCHtml site -> CCK site Html -> CC CCP (HandlerT site IO) Html
sendk html k = do
  (node, _) <- lift $ insertCCNode k
  lift $ $(logInfo) $ T.pack $ "sendk" ++ show node
  html node

inquire :: YesodCC site => CCHtml site -> CC CCP (HandlerT site IO) CCNode
inquire html =  shiftP p2R $ sendk html

inquireFinish ::  YesodCC site
                  => Html -> CC CCP (HandlerT site IO) Html
inquireFinish  html = abortP p2R $ return html

inquireGet :: (YesodCC site , FormEdge a)
              => CCNode
              -> CCHtml site
              -> (Html -> MForm (HandlerT site IO) (FormResult a, t))
              -> CC CCP (HandlerT site IO) CCLEdge
inquireGet node html form = do
  newNode  <- inquire html
  ((result, _widget), _enctype) <- lift $ runFormGet form
  lift $ insertCCEdge node newNode result

inquireGetUntil :: (YesodCC site, FormEdge a)
                   => CCNode
                   -> CCHtml site
                   -> (Html -> MForm (HandlerT site IO) (FormResult a, t))
                   -> CC CCP (HandlerT site IO) CCLEdge
inquireGetUntil node html form = do
  newNode <- inquire html
  ((result, _widget), _enctype) <- lift $ runFormGet form
  edge <- lift $ insertCCEdge node newNode result

  case result of
    FormSuccess _ -> return edge
    _             -> inquireGetUntil newNode html form


inquirePost :: (YesodCC site, FormEdge a, RenderMessage site FormMessage)
               => CCNode
               -> CCHtml site
               -> (Html -> MForm (HandlerT site IO) (FormResult a, t))
               -> CC CCP (HandlerT site IO) CCLEdge
inquirePost node html form = do
  newNode <- inquire html
  ((result, _widget), _enctype) <- lift $ runFormPost form
  lift $ insertCCEdge node newNode result


inquirePostUntil :: (YesodCC site, FormEdge a, RenderMessage site FormMessage)
                    => CCNode
                    -> CCHtml site
                    -> (Html -> MForm (HandlerT site IO) (FormResult a, t))
                    -> CC CCP (HandlerT site IO) CCLEdge
inquirePostUntil node html form = do
  newNode <- inquire html
  lift $ $(logInfo) $ T.pack $ "inquirePostUntil -> " ++ show newNode
  ((result, _widget), _enctype) <- lift $ runFormPost form
  edge <- lift $ insertCCEdge node newNode result

  case result of
    FormSuccess _ -> return edge
    _             -> inquirePostUntil newNode html form


runFormPostButtons :: (Show b) => [(Text,b)] -> (HandlerT site IO) (Maybe b)
runFormPostButtons [] = return Nothing
runFormPostButtons ((name,value):xs) = do
  p <- lookupPostParam name
  case p of
    Just _  -> return (Just value)
    _       -> runFormPostButtons xs


inquirePostButton :: (YesodCC site, FormEdge a, RenderMessage site FormMessage, Show a, Show b)
                     => CCNode
                     -> CCHtml site
                     -> (Html -> MForm (HandlerT site IO) (FormResult a, t))
                     -> [(Text,b)]
                     -> CC CCP (HandlerT site IO) (CCLEdge, Maybe b)
inquirePostButton node html form buttons = do
  edge  <- inquirePost node html form
  r <- lift $ runFormPostButtons buttons
  return (edge,r)


inquirePostUntilButton :: (YesodCC site, FormEdge a, RenderMessage site FormMessage, Show a, Show b)
                          => CCNode
                          -> CCHtml site
                          -> (Html -> MForm (HandlerT site IO) (FormResult a, t))
                          -> [(Text,b)]
                          -> CC CCP (HandlerT site IO) (CCLEdge, Maybe b)
inquirePostUntilButton node html form buttons = do
  edge <- inquirePostUntil node html form
  r    <- lift $ runFormPostButtons buttons
  let (_node, newNode, _la) = edge
  case r of
    Just _button -> return (edge, r)
    Nothing      -> inquirePostUntilButton newNode html form buttons


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


routePath :: YesodCC site => CCNode -> HandlerT site IO [CCLEdge]
routePath node = do
  gr <- readCCGraph
  routePath' gr node

    where
      routePath' gr node = do
        let ps = lpre gr node
        case ps of
          []    -> return []
          ((node',la ) :_) -> do
            route' <- routePath' gr node'
            return $ (node', node, la) : route'
