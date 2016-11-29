{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RankNTypes, NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings, OverloadedLabels, TypeApplications, ScopedTypeVariables #-}

module TestPlugin where

import Protolude hiding (get, set, on)

import GI.GLib (idleAdd)
import Foreign.Ptr (Ptr, FunPtr)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr_)
import GI.WebKit2WebExtension
import System.IO.Unsafe (unsafePerformIO)
import Foreign.C.Types (CULong(..))
import qualified GHC.Show as GS


type MouseHandler = DOMMouseEvent -> IO ()
type DOMElementModifier = DOMElement -> IO Bool

data VNode = Element
  { name :: Text
  , props :: [DOMElementModifier]
  , children :: [VNode]
  }
  | TextNode Text

instance GS.Show VNode where
  show (Element name _ _) = toS ("(Element " <> name <> ")")
  show (TextNode t) = toS ("(TextNode '" <> t <> "')")

onClick :: MouseHandler -> DOMElementModifier
onClick handler el = dOMEventTargetAddEventListener el "click" (mkMouseHandler handler) True

mkMouseHandler c = unsafePerformIO $
  wrapMouseEventClosureC (wrapMouseEventClosure c) >>= newCClosure

mkEventHandler c = unsafePerformIO $
  wrapEventClosureC (wrapEventClosure c) >>= newCClosure

testVNode :: Int -> VNode
testVNode n = div_ [onClick testClosure2] (replicate n (p_ [] [text_ "hello"]))

div_ :: [DOMElementModifier] -> [VNode] -> VNode
div_ mods children = Element "DIV" mods children

p_ :: [DOMElementModifier] -> [VNode] -> VNode
p_ = Element "P"

text_ :: Text -> VNode
text_ = TextNode

data DOMAPI = DOMAPI
  { createElement :: forall m. (MonadIO m) => VNode -> m DOMNode -- typically dOMDocumentCreateElement
  , appendChild :: forall m a b. (MonadIO m, IsDOMNode a, IsDOMNode b) => a -> b -> m DOMNode
  , replaceChild :: forall c b a m. (IsDOMNode c, IsDOMNode b, IsDOMNode a, MonadIO m) => a -> b -> c -> m DOMNode
  , setTextContent :: forall a m. (IsDOMNode a, MonadIO m) => a -> Text -> m ()
  }

childAt :: (IsDOMNode a, MonadIO m) => Int -> a -> m (Maybe DOMNode)
childAt n' node = do
  let n = CULong (fromIntegral n')
  children <- dOMNodeGetChildNodes node
  length' <- get children #length
  if n < length'
    then fmap Just (dOMNodeListItem children n)
    else pure Nothing

changed :: VNode -> VNode -> Bool
changed (Element name1 _ _) (Element name2 _ _) = name1 /= name2
changed (TextNode t1) (TextNode t2) = t1 /= t2
changed _ _ = True

safeIx :: (Integral n, Num n) => [a] -> n -> Maybe a
safeIx [] _ =
  Nothing
safeIx (x:xs) n
  | n == 0 = pure x
  | n < 0 = Nothing
  | otherwise = safeIx xs (n - 1)

-- https://github.com/bodil/purescript-vdom/blob/master/src/Data/VirtualDOM.purs
patch :: forall a. IsDOMNode a => DOMAPI -> a -> Maybe VNode -> Maybe VNode -> IO ()
patch api target old new = patchIndexed target old new 0
  where
    patchIndexed :: forall a2. IsDOMNode a2 =>  a2 -> Maybe VNode -> Maybe VNode -> Int -> IO ()
    patchIndexed _ Nothing Nothing _ = pure ()

    patchIndexed parent Nothing (Just new) _ = do
      el <- (createElement api) new
      void $ (appendChild api) parent el

    patchIndexed parent (Just _) Nothing index = do
      child' <- childAt index parent
      case child' of
        Just child -> void $ dOMNodeRemoveChild parent child
        Nothing -> pure ()

    patchIndexed parent (Just (TextNode old)) (Just (TextNode new)) index =
      when (old /= new) $ do
        me <- childAt index parent
        maybe (pure ()) (\me' -> (setTextContent api) me' new) me

    patchIndexed parent (Just old) (Just new) index = do
      me' <- childAt index parent
      case me' of
        Nothing -> pure ()
        Just me ->
          if (changed old new) then do
            n <- (createElement api) new -- TODO
            void $ (replaceChild api) parent n me
          else do
            --case (old, new) of
              --Element {props: oldProps}, Element {props: newProps} ->
              --  updateProps api me oldProps newProps
              --_, _ -> pure unit -- TODO props
            walkChildren me old new

    walkChildren :: forall a2. IsDOMNode a2 =>  a2 -> VNode -> VNode -> IO ()
    walkChildren target' (Element _ _ oldChildren) (Element _ _ newChildren) = do
      let r = [0 .. ((max (length oldChildren) (length newChildren)) - 1)]
      sequence_ $ map (\i -> patchIndexed target' (safeIx oldChildren i) (safeIx newChildren i) i) r
    walkChildren _ _ _ = pure ()

newDOMAPI :: DOMDocument -> DOMAPI
newDOMAPI doc = DOMAPI
  { createElement = create
  , appendChild = dOMNodeAppendChild
  , replaceChild = dOMNodeReplaceChild
  , setTextContent = dOMNodeSetTextContent
  }
  where
    create :: forall m. (MonadIO m) => VNode -> m DOMNode
    create (Element name' _ children') = do
      node <- dOMDocumentCreateElement doc name'
      Just me <- liftIO (castTo DOMNode node)
      -- TODO listeners and attributes (listeners can not be removed for now)
      sequence_ $ map (\child -> do
                          new'' <- create child
                          dOMNodeAppendChild me new''
                      ) children'
      pure me
    create (TextNode t) = do
      node <- dOMDocumentCreateTextNode doc t
      Just me <- liftIO (castTo DOMNode node)
      pure me

pageCreated :: WebPage -> IO ()
pageCreated page = do
  webPageGetId page >>= print

  void $ on page #documentLoaded $ do
    get page #uri >>= print
    doc <- webPageGetDomDocument page
    body <- dOMDocumentGetBody doc
    docEl <- dOMDocumentGetDocumentElement doc
    state <- newMVar (Nothing, 2)

    -- NB without the forkOS this function never finishes and the
    -- render loop never starts. requires -threaded
    let rm = renderMore (newDOMAPI doc) body state

    -- Somewhat crazily the document only renders correctly if we
    -- modify it in an event handler. I suspect this has to do with
    -- some "refresh" function we need to call but I haven't figured it out yet.
    -- void $ dOMEventTargetAddEventListener body "click" (mkEventHandler rm) True
    void $ forkOS $ forever $ do
      threadDelay (1000 * 1000)
      idleAdd 0 rm -- <-- this is the magic function!

      where
        renderMore :: DOMAPI -> DOMHTMLElement -> MVar (Maybe VNode, Int) -> IO Bool
        renderMore api body state  = do
          (vdom, n) <- takeMVar state
          print n
          let newVDom = Just (testVNode n)
          putMVar state (newVDom, n + 1)
          patch api body vdom newVDom
          pure False


pageCreated_hs :: Ptr () -> Ptr WebPage -> Ptr () -> IO ()
pageCreated_hs = webExtensionPageCreatedCallbackWrapper pageCreated

foreign export ccall pageCreated_hs :: WebExtensionPageCreatedCallbackC

testClosure2 :: DOMMouseEvent -> IO ()
testClosure2 ev = do
  Just doc <- dOMEventGetSrcElement ev >>= castTo DOMElement
  x <- dOMMouseEventGetClientX ev
  y <- dOMMouseEventGetClientY ev
  set doc [#innerHtml := ("<h2>Mouse</h2>x: " <> (show x :: Text) <> ", y: " <> (show y :: Text))]
  pure ()

type EventHandlerCClosure a = Ptr () -> Ptr a -> IO ()

wrapEventClosure :: (DOMEvent -> IO ()) -> EventHandlerCClosure DOMEvent
wrapEventClosure handler _ e = do
  e' <- newForeignPtr_ e
  let event = DOMEvent e'
  handler event

wrapMouseEventClosure :: (DOMMouseEvent -> IO ()) -> EventHandlerCClosure DOMMouseEvent
wrapMouseEventClosure handler _ e = do
  e' <- newForeignPtr_ e
  let event = DOMMouseEvent e'
  handler event

foreign import ccall "wrapper" wrapEventClosureC :: EventHandlerCClosure DOMEvent -> IO (FunPtr (EventHandlerCClosure DOMEvent))
foreign import ccall "wrapper" wrapMouseEventClosureC :: EventHandlerCClosure DOMMouseEvent -> IO (FunPtr (EventHandlerCClosure DOMMouseEvent))
