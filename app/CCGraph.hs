{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE UndecidableInstances  #-}
module CCGraph (
  CCPrologT(..),
  CCPrologHandlerT,
  CCK,
  CCContentType(..),
  CCContentTypeM,
  CCFormResult(..),
  CCP,
  CCNode,
  CCNodeLabel(..),
  CCEdgeLabel(..),
  CCLNode,
  CCLEdge,
  CCGraph,
  CCStateT, ccsCurrentNode, ccsCurrentForm, ccsCurrentRoute,
  CCDatabase,
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
  lookupCCNodeTitle,
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
  getFormMissingState,
  getFormSuccessState
  ) where

import             Import.NoFoundation
import             Control.Monad.Reader
import             Control.Monad.State
import             Data.Monoid
import             Text.Blaze (Markup)
import             Control.Monad.CC.CCCxe
import qualified   Data.Text as T
import qualified   Data.List as List
import             Data.Tuple
import             Data.Time.LocalTime
import             Data.Graph.Inductive.Graph
import qualified   Data.Graph.Inductive.Graph as Graph
import             Data.Graph.Inductive.PatriciaTree
import             Data.Typeable
import             Form
import             ShowText
import             Language.Prolog2.Types
import qualified   Language.Prolog2.Database as DB
import             Language.Prolog2.Database(Database)

type CCP                 = PP


newtype CCPrologT site m a    =
  CCPrologT { unCCPrologT :: CC CCP (PrologDatabaseT (CCStateT site) (CCPrologT site m) m) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader (Database (CCStateT site) (CCPrologT site m))
           , MonadState (IntBindingState T)
           )



type CCPrologHandlerT site a  = CCPrologT site (HandlerT site IO) a

data CCFormResult   = forall a. (Show a, Eq a, Typeable a) =>
                      CCFormResult (FormResult a)

type CCNode              = Graph.Node
data CCContentType       = CCContentHtml Html | CCContentJson Value

type CCContentTypeM site = CCNode -> CCPrologHandlerT site CCContentType

data CCStateT site        = CCStateT { ccsCurrentNode :: CCNode
                                   , ccsCurrentForm :: CCFormResult
                                   , ccsCurrentRoute :: Route site
                                   }

type CCDatabase site     = Database (CCStateT site) (CCPrologT site (HandlerT site IO))
type CCK site w          = CCStateT site -> CCPrologHandlerT site w
data CCNodeLabel site    =
  CCNodeLabel { ccTimestamp :: LocalTime
              , ccK         :: Maybe (CCK site CCContentType)
              , ccKArg      :: CCStateT site
              , ccDatabase  :: CCDatabase site
              , ccNodeTitle :: Text
              }
data CCEdgeLabel site    = CCEdgeLabel { ccRoute     :: Route site
                                       , ccResponse  :: CCFormResult }
                           deriving ( Typeable)
type CCLNode site        = LNode (CCNodeLabel site)
type CCLEdge site        = LEdge (CCEdgeLabel site)
type CCGraph site        = Gr (CCNodeLabel site) (CCEdgeLabel site)

type PathPieces = ([Text], [(Text,Text)])

----------------------  Define the type class ------------------------
deriving instance (RenderRoute site) => Eq (CCEdgeLabel site)

class Typeable site => YesodCC site where
  getCCPool :: site -> MVar (CCGraph site)

  newCCPool :: IO (MVar (CCGraph site))
  newCCPool = newMVar Graph.empty

--  takeCCGraph ::  HandlerT site IO (CCGraph site)
  readCCGraph ::  HandlerT site IO (CCGraph site)
  modifyCCGraph :: (CCGraph site -> IO (CCGraph site, b) ) -> HandlerT site IO b

  getBuiltinDatabase :: HandlerT site IO (CCDatabase site)

instance  MonadTrans (CCPrologT site) where
  lift = CCPrologT . lift . lift

instance  MonadIO m => MonadIO (CCPrologT site m) where
  liftIO = lift . liftIO

instance  MonadProlog (CCPrologT site) where
  liftProlog = CCPrologT . lift . liftProlog

instance (Monad m)  => MonadPrologDatabase (CCPrologT site) (CCStateT site) (CCPrologT site m) m  where
  liftPrologDatabase = CCPrologT . lift

instance (RenderRoute site, Show (Route site)) => Show (CCNodeLabel site) where
  show (CCNodeLabel time (Just _) res db title)  = show (title, time, "Just <cont>",res, "<db>")
  show (CCNodeLabel time (Nothing) res db title) = show (title, time, "Nothing", res , "<db>")


instance (RenderRoute site, Show (Route site)) => ShowText (CCNodeLabel site) where
  showT (CCNodeLabel time (Just _) res db title)  =
    showT ( title,  T.pack $ show time, T.pack "Just <cont>", T.pack $ show res, T.pack "<db>" )
  showT (CCNodeLabel time (Nothing) res db title) =
    showT ( title, T.pack $ show time, T.pack "Nothing", T.pack $ show res , T.pack "<db>" )

instance (RenderRoute site, Show (Route site)) => Show (CCStateT site) where
  show (CCStateT node form  route)  = "CCStateT " ++ show (node,form, route)

--instance RenderRoute site => Show (Route site) where
--  show route = show $ renderRoute route

deriving instance  (RenderRoute site, Show (Route site)) => Show (CCEdgeLabel site)


deriving instance Typeable CCEdgeLabel

instance Eq CCFormResult where
  CCFormResult a == CCFormResult a' =  cast a == Just a'


deriving instance Show     CCFormResult
deriving instance Typeable CCFormResult

instance MonadLogger m => MonadLogger (CC p m)
instance MonadLogger m => MonadLogger (PrologDatabaseT site n m)
instance MonadLogger m => MonadLogger (CCPrologT site m)

updateState :: CCStateT site -> CCStateT site -> CCStateT site
updateState (CCStateT node form route) (CCStateT node' form' route')
  | node == node' = CCStateT node' (form `updateForm` form') route'
  | otherwise     = error $ show (node,node')
  where
    _ `updateForm` f@(CCFormResult (FormSuccess a)) = f
    f@(CCFormResult (FormSuccess a)) `updateForm` _ = f
    f `updateForm` f' = f'

---------------------- - Running continuations  ----------------------

run ::  Typeable a =>
        CCPrologHandlerT site a -> CCDatabase site
        ->  HandlerT site IO (Either RuntimeError a , IntBindingState T)
run (CCPrologT m) db = do
  $(logInfo) $ T.pack $ "Running a new continuation"

  runPrologDatabaseT (runCC $ pushPrompt pp m) db

resume ::  YesodCC site
           =>  CCStateT site
           -> HandlerT site IO (Either RuntimeError CCContentType, IntBindingState T)
resume st@(CCStateT node form route) = do
  $(logInfo) $ T.pack $ "resuming:" ++ show node
  mlabel <- lookupCCNodeLabel node
  case mlabel of
    Just (CCNodeLabel _ (Just k) st' db title) -> do
      let st'' = st' `updateState` st
      runPrologDatabaseT (runCC $ unCCPrologT $ k st'') db
    Just (CCNodeLabel _ Nothing st' db title) -> do
      $(logInfo) $ T.pack $ "Continuation not found:" ++ show node ++ ":" ++ show title
      setMessage $ toHtml $ T.pack  $ "Continuation has expired"
      notFound
    Nothing -> do
      $(logInfo) $ T.pack $ "Undefined node:" ++ show node
      setMessage $ toHtml $ T.pack  $ "Continuation has expired"
      notFound


-------------- Access to the global continuation store  --------------


insertCCNode :: forall site. (YesodCC site, RenderRoute site, Show (Route site))
                => CCK site CCContentType -> Text -> CCFormResult -> Route site -> CCDatabase site
                -> HandlerT site IO (CCLNode site)
insertCCNode k title form route db = do
  ZonedTime time _tz <- lift $ getZonedTime
  lnode <- modifyCCGraph $ f time
  $(logInfo) $ T.concat [ "insertCCNode: " ,  showT lnode ]
  return lnode
    where
      f :: LocalTime -> CCGraph site -> IO (CCGraph site, CCLNode site)
      f time gr = do
        let [newNode] = newNodes 1 gr
            newLNode  = (newNode, CCNodeLabel time (Just k) (CCStateT newNode form route) db title)
        return (insNode newLNode gr, newLNode)

insertCCRoot :: forall site . (YesodCC site, RenderRoute site, Show (Route site))
                => Text ->  CCFormResult -> Route site -> CCDatabase site
                -> HandlerT site IO (CCLNode site)
insertCCRoot title form route db = do
  ZonedTime time _tz <- lift $ getZonedTime
  lnode <- modifyCCGraph $ f time :: HandlerT site IO (CCLNode site)
  $(logInfo) $ T.pack $ "insertCCRoot: " ++ show lnode
  db <- getBuiltinDatabase
  return lnode
    where
      f :: LocalTime -> CCGraph site -> IO (CCGraph site, CCLNode site)
      f time gr = do let [newNode] = newNodes 1 gr
                         newLNode  = (newNode, CCNodeLabel time (Nothing :: Maybe (CCK site CCContentType))
                                               (CCStateT newNode form route)
                                               db
                                               title
                                     )
                     return (insNode newLNode gr, newLNode)


insertCCEdge :: (YesodCC site , RenderRoute site , Show (Route site))
                => CCNode -> CCNode -> CCFormResult -> Route site -> HandlerT site IO (CCLEdge site)
insertCCEdge node newNode form route = do
  edge <- modifyCCGraph f
  $(logInfo) $ T.pack $ "insertCCEdge: " ++ show edge
  return edge

    where
      f gr = do let newLEdge  = (node, newNode, CCEdgeLabel route form)
                return (insEdge newLEdge gr, newLEdge)

insertCCLEdge :: (YesodCC site, RenderRoute site , Show (Route site) )
                 => CCLEdge site -> HandlerT site IO (CCLEdge site)
insertCCLEdge e = do
  edge <- modifyCCGraph f
  $(logInfo) $ T.pack $ "insertCCLEdge: " ++ show edge
  return edge
    where
      f gr = return (insEdge e gr, e)

delCCLEdge :: (YesodCC site, RenderRoute site, Show (Route site))
              => CCLEdge site -> HandlerT site IO (CCGraph site)
delCCLEdge edge = do
  gr <- modifyCCGraph f
  $(logInfo) $ T.pack $ "delCCLEdge: " ++ show edge
  return gr
    where
      f gr = do let gr' = delLEdge edge gr
                return (gr',gr')


lookupSuc :: (YesodCC site, RenderRoute site, Show (Route site))
             => CCStateT site -> HandlerT site IO (Maybe CCNode)
lookupSuc (CCStateT node response route) = do
  gr <- readCCGraph
  let ss = map swap $ Graph.lsuc gr node
  $logInfo $ T.pack $ show response
  $logInfo $ T.pack $ show ss
  return $ List.lookup (CCEdgeLabel route response) ss


lookupCCK :: (YesodCC site, Typeable site) => CCNode -> HandlerT site IO (Maybe (CCK site CCContentType))
lookupCCK node = do
  gr <- readCCGraph
  case Graph.lab gr node of
    Just (CCNodeLabel _ (Just cck) _ _ _) -> return (Just cck)
    _                                   -> return Nothing

lookupCCNodeLabel :: (YesodCC site, Typeable site) => CCNode -> HandlerT site IO (Maybe (CCNodeLabel site))
lookupCCNodeLabel node = do
  gr <- readCCGraph
  return $ Graph.lab gr node

lookupCCNodeTitle :: (YesodCC site, Typeable site) => CCNode -> HandlerT site IO (Maybe Text)
lookupCCNodeTitle node = do
  gr <- readCCGraph
  return $ ccNodeTitle <$> Graph.lab gr node


updateCCKArg :: forall site. (YesodCC site, Typeable site)
                => CCStateT site -> HandlerT site IO (Maybe (CCNodeLabel site))
updateCCKArg st =
  modifyCCGraph f
  where
    f :: CCGraph site -> IO (CCGraph site, Maybe (CCNodeLabel site))
    f gr = do
      case Graph.match (ccsCurrentNode st) gr of
        (Just (in', node' , lab' , out'), gr') -> do let lab'' = lab' { ccKArg = st}
                                                         cxt'' = (in',node',lab'',out')
                                                         gr''  = cxt'' & gr'
                                                     return (gr'', Just lab'')
        _ -> return (gr,Nothing)

---------------------- Continuation primitives  ----------------------
sendk ::  (YesodCC site, RenderRoute site, Show (Route site))
          => CCStateT site -> Text -> CCContentTypeM site -> CCK site CCContentType
          -> CCPrologHandlerT site  CCContentType
sendk state@(CCStateT node response route) title content k = do
  $(logInfo) $ T.pack $ show "sendk:" ++ show node

  db  <- ask
  msuc <- lift $ lookupSuc state
  suc <- case (msuc, response) of
    (Just suc,  _ ) -> do
      $(logInfo) $ T.pack $ "sendk: edge exists:" ++ show (suc, response)
      return suc
    (Nothing , CCFormResult (FormSuccess a) )  -> do
      (new, _) <- lift $ insertCCNode k title response route db
      lift $ insertCCLEdge (node, new, CCEdgeLabel  route response)
      return new

    (Nothing , CCFormResult _ ) -> do
      $(logInfo) $ T.pack $ "sendk: invalid form:" ++ show (response)
      return node


  $(logInfo) $ T.pack $ "sendk: suc=" ++ show suc

  content suc

inquire :: (YesodCC site, RenderRoute site , Show (Route site))
           => CCStateT site -> Text -> CCContentTypeM site -> CCPrologHandlerT site  (CCStateT site)
inquire st title content  = do
  st' <- CCPrologT $ shiftP pp $ \k -> (unCCPrologT $ sendk st title content (CCPrologT . k))

  $(logInfo) $ T.pack $ "inquire:" ++ show st'

  lift $ updateCCKArg st'
  return st'

inquireFinish ::  YesodCC site => CCContentType -> CCPrologHandlerT site CCContentType
inquireFinish  content = do
  $logInfo $ T.pack $ "inquireFinish:"
  CCPrologT $ abortP pp $ return content

inquireGet :: (YesodCC site, RenderRoute site, Show (Route site) , Eq a, Show a, Typeable a)
              => CCStateT site
              -> Text
              -> CCContentTypeM site
              -> (Html -> MForm (HandlerT site IO) (FormResult a, t))
              -> CCPrologHandlerT site (CCStateT site)
inquireGet st title html form = do
  st'   <- inquire st title html
  ((result, _widget), _enctype) <- lift $ runFormGet form
  $logInfo $ T.pack $ "inquireGet:" ++ show result
  return $ st' { ccsCurrentForm = CCFormResult result }

inquireGetUntil :: (YesodCC site, RenderRoute site, Show (Route site), Eq a,Show a,Typeable a)
                   => CCStateT site
                   -> Text
                   -> CCContentTypeM site
                   -> (Html -> MForm (HandlerT site IO) (FormResult a, t))
                   -> CCPrologHandlerT site  (CCStateT site)
inquireGetUntil st title html form = do
  st' <- inquire st title html
  ((result, _widget), _enctype) <- lift $ runFormGet form
  $logInfo $ T.pack $ "inquireGetUntil:" ++ show result

  let st'' = st' { ccsCurrentForm = CCFormResult result }
  case result of
    FormSuccess _ -> return $ st''
    _             -> inquireGetUntil st'' title html form


inquirePost :: (YesodCC site,  RenderRoute site, RenderMessage site FormMessage, Show (Route site)
               , Eq a, Show a, Typeable a )
               => CCStateT site
               -> Text
               -> CCContentTypeM site
               -> (Html -> MForm (HandlerT site IO) (FormResult a, t))
               -> CCPrologHandlerT site (CCStateT site)
inquirePost st title html form = do
  st' <- inquire st title html
  ((result, _widget), _enctype) <- lift $ runFormPost form
  let st'' = st' { ccsCurrentForm = CCFormResult result }
  return $ st''

inquirePostUntil :: (YesodCC site, RenderRoute site, Show (Route site), RenderMessage site FormMessage
                    , Eq a, Show a, Typeable a)
                    => CCStateT site
                    -> Text
                    -> CCContentTypeM site
                    -> (Html -> MForm (HandlerT site IO) (FormResult a, t))
                    -> CCPrologHandlerT site  (CCStateT site)
inquirePostUntil st title html form = do
  st' <- inquire st  title html
  $(logInfo) $ T.pack $ "inquirePostUntil -> " ++ show st'
  ((result, _widget), _enctype) <- lift $ runFormPost form
  let st'' = st' { ccsCurrentForm = CCFormResult result }
  case result of
    FormSuccess _ -> return st''
    _             -> inquirePostUntil st'' title html form


runFormPostButtons :: (Show b) => [(Text,b)] -> (HandlerT site IO) (Maybe b)
runFormPostButtons [] = return Nothing
runFormPostButtons ((name,value):xs) = do
  p <- lookupPostParam name
  case p of
    Just _  -> return (Just value)
    _       -> runFormPostButtons xs


inquirePostButton :: (YesodCC site, RenderRoute site, RenderMessage site FormMessage, Show (Route site)
                     , Show a, Eq a, Typeable a, Show b)
                     => CCStateT site
                     -> Text
                     -> CCContentTypeM site
                     -> (Html -> MForm (HandlerT site IO) (FormResult a, t))
                     -> [(Text,b)]
                     -> CCPrologHandlerT site (CCStateT site, Maybe b)
inquirePostButton st title html form buttons = do
  st'  <- inquirePost st title html form
  r    <- lift $ runFormPostButtons buttons
  return (st', r)


inquirePostUntilButton :: (YesodCC site,  RenderRoute site, RenderMessage site FormMessage, Show (Route site)
                          , Eq a, Typeable a, Show a, Show b)
                          => CCStateT site
                          -> Text
                          -> CCContentTypeM site
                          -> (Html -> MForm (HandlerT site IO) (FormResult a, t))
                          -> [(Text,b)]
                          -> CCPrologHandlerT site  (CCStateT site, Maybe b)
inquirePostUntilButton st title html form buttons = do
  st'  <- inquirePostUntil st title html form
  r    <- lift $ runFormPostButtons buttons

  case r of
    Just _button -> return (st', r)
    Nothing      -> inquirePostUntilButton st' title html form buttons


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


rootPath :: YesodCC site => CCNode -> HandlerT site IO [CCLEdge site]
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


lowerLeftPath :: YesodCC site => CCNode -> HandlerT site IO [CCLEdge site]
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


spine :: YesodCC site => CCNode -> HandlerT site IO [CCLEdge site]
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

getFormMissingState :: CCNode -> HandlerT site IO (CCStateT site)
getFormMissingState node = do
  mroute <- getCurrentRoute
  case mroute of
    Just route -> do
      return $ CCStateT node (CCFormResult (FormMissing :: FormResult ())) route
    Nothing -> do
      notFound

getFormSuccessState :: (Eq a, Show a, Typeable a) => CCNode ->  a -> HandlerT site IO (CCStateT site)
getFormSuccessState node a = do
  mroute <- getCurrentRoute
  case mroute of
    Just route -> do
      return $ CCStateT node (CCFormResult (FormSuccess a)) route
    Nothing -> do
      notFound
