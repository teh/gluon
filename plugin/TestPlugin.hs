{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RankNTypes, NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings, OverloadedLabels, TypeApplications, ScopedTypeVariables #-}

module TestPlugin where

import Protolude hiding (get, set, on) -- (Text, toS, (<>), void, MonadIO, Word64, when, liftIO, traceM, show)

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr_)
import GI.WebKit2WebExtension
import System.IO.Unsafe (unsafePerformIO)
import Foreign.C.Types (CULong(..))


type MouseHandler = DOMMouseEvent -> IO ()
type DOMElementModifier = DOMElement -> IO Bool

data VNode = Element
  { name :: Text
  , props :: [DOMElementModifier]
  , children :: [VNode]
  }
  | TextNode Text

onClick :: MouseHandler -> DOMElementModifier
onClick handler el = dOMEventTargetAddEventListener el "click" (mkMouseHandler handler) True

mkMouseHandler c = unsafePerformIO $
  wrapMouseEventClosureC (wrapMouseEventClosure c) >>= newCClosure

testVNode :: VNode
testVNode = div_ [onClick testClosure2] [text_ "hello"]

div_ :: [DOMElementModifier] -> [VNode] -> VNode
div_ mods children = Element "div" mods children

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
safeIx [] _ = Nothing
safeIx (x:xs) n | n == 0 = pure x
                | n < 0 = Nothing
                | otherwise = safeIx xs (n-1)

-- https://github.com/bodil/purescript-vdom/blob/master/src/Data/VirtualDOM.purs
patch :: forall a. IsDOMNode a => DOMAPI -> a -> Maybe VNode -> Maybe VNode -> IO ()
patch api target old new = patchIndexed target old new 0
  where
    patchIndexed :: forall a. IsDOMNode a =>  a -> Maybe VNode -> Maybe VNode -> Int -> IO ()
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

    walkChildren :: forall a. IsDOMNode a =>  a -> VNode -> VNode -> IO ()
    walkChildren target (Element _ _ oldChildren) (Element _ _ newChildren) = do
      let r = [0 .. ((max (length oldChildren) (length newChildren)) - 1)]
      traceM ("WALK " <> show r)
      sequence_ $ map (\i -> patchIndexed target (safeIx oldChildren i) (safeIx newChildren i) i) r
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
    create (Element name _ children) = do
      node <- dOMDocumentCreateElement doc name
      Just r <- liftIO (castTo DOMNode node)

      sequence_ $ map (\child -> do
                          new <- create child
                          dOMNodeAppendChild r new
                      ) children

      traceM ("element" <> name)
      pure r
    create (TextNode t) = do
      node <- dOMDocumentCreateTextNode doc t
      Just r <- liftIO (castTo DOMNode node)
      traceM ("textnode" <> t)
      pure r

pageCreated :: WebPage -> IO ()
pageCreated page = do
  webPageGetId page >>= print
  void $ on page #documentLoaded $ do
    get page #uri >>= print
    doc <- webPageGetDomDocument page
    body <- dOMDocumentGetBody doc
    patch (newDOMAPI doc) body Nothing (Just testVNode)
    get body #innerHtml >>= print

pageCreated_hs :: Ptr () -> Ptr WebPage -> Ptr () -> IO ()
pageCreated_hs = webExtensionPageCreatedCallbackWrapper pageCreated

foreign export ccall pageCreated_hs :: WebExtensionPageCreatedCallbackC

testClosure :: DOMEvent -> IO ()
testClosure _ = do
  print "testClosure"

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
