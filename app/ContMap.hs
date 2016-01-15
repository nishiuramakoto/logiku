
module ContMap (
  GeneralContinuation,
  Cont,
  ContId,
  ContMap,
  YesodCC(..),
  run,
  resume,
  answerGet,
  readContMap,
  modifyContMap,
  insertContMap,
  lookupContMap,
  inquire,
  inquireFinish,
  inquireGetUntil,
  inquirePostUntil,
  inquirePostUntilButton,
  generateKFormGet,
  generateKFormPost,
  generateKlabel,
  ) where

-- import             Import
import             Yesod
import             Prelude
import             Text.Blaze (Markup)
import             Data.Unique
import             Data.IORef
import             Data.Map(Map)
import qualified   Data.Map as Map
import             Data.Text(Text)
import qualified   Data.Text as T
import             Control.Monad.CC.CCCxe



-- type AppHandler = HandlerT App IO
type GeneralContinuation m a = a -> CC (PS a) m a
type Cont site  = GeneralContinuation (HandlerT site IO)  Html
type ContId     = Int
type ContMap site = Map ContId (Cont site)


-- Missing instances
-- instance Monad m => Functor (CC p m) where
--   fmap f cc = do a <- cc
--                  return (f a)

-- instance Monad m => Applicative (CC p m) where
--   pure     = return
--   m <*> x  = do a <- m
--                 b <- x
--                 return (a b)

----------------------  Define the type class ------------------------
class YesodCC site where
  getCcPool :: site -> IORef (ContMap site)

  newCcPool :: IO (IORef (ContMap site))
  newCcPool = newIORef Map.empty
-------------- Access to the global continuation store  --------------



readContMap :: YesodCC site => HandlerT site IO  (ContMap site)
readContMap = do
  yesod <- getYesod
  liftIO $ readIORef $ getCcPool yesod

modifyContMap :: YesodCC site => ( (ContMap site) -> (ContMap site) ) -> HandlerT site IO ()
modifyContMap f = do
  yesod <- getYesod
  let ioref = getCcPool yesod
  liftIO $ modifyIORef' ioref f

insertContMap :: YesodCC site => ContId -> Cont site -> HandlerT site IO ()
insertContMap klabel k =
  modifyContMap (Map.insert klabel k)

lookupContMap :: YesodCC site => ContId -> HandlerT site IO (Maybe (Cont site))
lookupContMap klabel = do
  cont_map <- readContMap
  return $ Map.lookup klabel cont_map

---------------------- Continuation primitives  ----------------------
sendk :: YesodCC site
         => ContId
         -> Html
         -> (Html -> CC (PS Html) (HandlerT site IO) Html)
         -> CC (PS Html) (HandlerT site IO) Html
sendk klabel html k = do
  lift $ insertContMap klabel k
  return html

inquire :: YesodCC site => ContId -> Html -> CC (PS Html) (HandlerT site IO) Html
inquire klabel html = shiftP ps $ sendk klabel html

inquireFinish :: YesodCC site => Html -> CC (PS Html) (HandlerT site IO) Html
inquireFinish html = abortP ps $ return html


inquireGetUntil :: YesodCC site => ContId -> Html -> (Html -> MForm (HandlerT site IO) (FormResult a, t))
                   -> CC (PS Html) (HandlerT site IO) a
inquireGetUntil klabel html form = do
  _ <- inquire klabel html
  ((result, _widget), _enctype) <- lift $ runFormGet form
  case result of
    FormSuccess r -> return r
    _             -> inquireGetUntil klabel html form

inquirePostUntil :: (YesodCC site, RenderMessage site FormMessage)
                    => ContId -> Html -> (Html -> MForm (HandlerT site IO) (FormResult a, t))
                    -> CC (PS Html) (HandlerT site IO) a
inquirePostUntil klabel html form = do
  _ <- inquire klabel html
  ((result, _widget), _enctype) <- lift $ runFormPost form
  case result of
    FormSuccess r -> return r
    _             -> inquirePostUntil klabel html form

inquirePostUntilButton :: (YesodCC site, RenderMessage site FormMessage, Show a, Show b)
                          => ContId -> Html ->  (Html -> MForm (HandlerT site IO) (FormResult a, t))
                          -> [(Text,b)]
                          -> CC (PS Html) (HandlerT site IO) (a, Maybe b)
inquirePostUntilButton klabel html form buttons = do
  x <- inquirePostUntil klabel html form
  r <- lift $ runFormPostButtons buttons
  case r of
    Just _button -> return (x, r)
    Nothing     -> inquirePostUntilButton klabel html form buttons


runFormPostButtons :: (YesodCC site, Show b)
                      => [(Text,b)] -> (HandlerT site IO) (Maybe b)
runFormPostButtons [] = return Nothing
runFormPostButtons ((name,value):xs) = do
  $(logInfo) ("inquireFormPostButtons" `T.append` T.pack (show name) )
  p <- lookupPostParam name
  $(logInfo) "inquireFormPostButtons"
  case p of
    Just _  -> do
      $(logInfo) $ "inquireFormPostButtons" `T.append` T.pack (show value)
      return (Just value)
    _      -> runFormPostButtons xs


-- answerGet :: (MonadTrans t, Monad (t AppHandler))
--             => (Html -> MForm AppHandler (FormResult a, Widget)) -> t AppHandler a -> t AppHandler a
answerGet :: (MonadTrans t, MonadHandler m, Monad (t m)) =>
             (Markup -> MForm m (FormResult a, t1)) -> t m a -> t m a

answerGet form error_action = do
  ((result, _widget), _enctype) <- lift $ runFormGet form

  case result of
    FormSuccess r -> return r
    _             -> error_action



run :: (YesodCC site) => (CC (PS Html) (HandlerT site IO) Html) ->  (HandlerT site IO) Html
run f = runCC $ pushPrompt ps f

resume :: YesodCC site =>  ContId -> Html -> Html -> (HandlerT site IO) Html
resume klabel cont_html not_found = do
   maybe_k <- lookupContMap klabel
   case maybe_k of
     Just k  -> runCC $ k cont_html
     Nothing -> return not_found



----------------------------  Yesod defs  ----------------------------
generateKFormGet ::  (RenderMessage (HandlerSite m) FormMessage, MonadHandler m)
                     => (Markup -> MForm m (FormResult a, xml)) -> m (Int, xml, Enctype)
generateKFormGet form = do
  (widget, enctype) <- generateFormGet' form
  klabel <- generateKlabel
  return (klabel, widget, enctype)

generateKFormPost ::  (RenderMessage (HandlerSite m) FormMessage, MonadHandler m)
                     => (Markup -> MForm m (FormResult a, xml)) -> m (Int, xml, Enctype)
generateKFormPost form = do
  (widget, enctype) <- generateFormPost form
  klabel <- generateKlabel
  return (klabel, widget, enctype)

generateKlabel ::  (RenderMessage (HandlerSite m) FormMessage, MonadHandler m)
                     => m Int
generateKlabel = do
  unique <- liftIO $ newUnique
  let klabel = hashUnique unique
  return klabel
