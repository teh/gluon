{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RankNTypes, NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}

module Gluon.VDom
  ( VNode(..)
  , childAt
  , patch
  , newDOMAPI
  , DOMAPI
  , MouseHandler
  , KeyboardHandler
  , onClick
  , DOMElementModifier
  ) where

import Protolude hiding (get, on)
import qualified GHC.Show as GS
import GI.WebKit2WebExtension (IsDOMNode, DOMDocument, DOMNode(..), dOMNodeGetChildNodes,
                               dOMNodeListItem, dOMNodeRemoveChild, dOMNodeSetTextContent,
                               dOMNodeReplaceChild, dOMNodeAppendChild, dOMDocumentCreateElement,
                               dOMDocumentCreateTextNode, DOMMouseEvent(..), DOMElement, DOMKeyboardEvent(..),
                               DOMEvent(..), dOMEventTargetAddEventListener, dOMElementSetAttribute,
                               IsDOMElement, dOMElementRemoveAttribute, DOMElement(..))
import Foreign.C.Types (CULong(..))
import GI.GLib (get, castTo, Closure, newCClosure)
import System.IO.Unsafe (unsafePerformIO)
import Foreign.Ptr (Ptr, FunPtr)
import Foreign.ForeignPtr (newForeignPtr_)
import qualified Data.Map as Map

type EventHandler = DOMEvent -> IO ()
type MouseHandler = DOMMouseEvent -> IO ()
type KeyboardHandler = DOMKeyboardEvent -> IO ()

-- TODO move DOMElementModifier to an "Internal" module
type DOMElementModifier = DOMElement -> IO Bool

onClick :: MouseHandler -> Either (Text, Text) DOMElementModifier
onClick handler = Right (\el -> dOMEventTargetAddEventListener el "click" (mkMouseHandler handler) True)


data VNode = Element
  { tagName :: Text
  , props :: [(Text, Text)]
  , listeners :: [DOMElementModifier]
  , children :: [VNode]
  }
  | TextNode Text

-- Show instance breaks the show/read contract, only use for
-- debugging!
instance GS.Show VNode where
  show (Element tagName' _ _ children') = toS ("(Element " <> tagName' <> " (" <> (show children')  <> "))")
  show (TextNode t) = toS ("(TextNode '" <> t <> "')")


data DOMAPI = DOMAPI
  { createElement :: forall m. (MonadIO m) => VNode -> m DOMNode -- typically dOMDocumentCreateElement
  , appendChild :: forall m a b. (MonadIO m, IsDOMNode a, IsDOMNode b) => a -> b -> m DOMNode
  , replaceChild :: forall c b a m. (IsDOMNode c, IsDOMNode b, IsDOMNode a, MonadIO m) => a -> b -> c -> m DOMNode
  , setTextContent :: forall a m. (IsDOMNode a, MonadIO m) => a -> Text -> m ()
  , setAttribute :: forall a m. (IsDOMElement a, MonadIO m) => a -> Text -> Text -> m ()
  , removeAttribute :: forall a m. (IsDOMElement a, MonadIO m) => a -> Text -> m ()
  }


childAt :: (IsDOMNode a, MonadIO m) => Int -> a -> m (Maybe DOMNode)
childAt n' node = do
  let n = CULong (fromIntegral n')
  children' <- dOMNodeGetChildNodes node
  length' <- get children' #length
  if n < length'
    then fmap Just (dOMNodeListItem children' n)
    else pure Nothing

changed :: VNode -> VNode -> Bool
changed (Element tagName1 _ _ _) (Element tagName2 _ _ _) = tagName1 /= tagName2
changed (TextNode t1) (TextNode t2) = t1 /= t2
changed _ _ = True


safeIx :: (Integral n) => [a] -> n -> Maybe a
safeIx [] _ =
  Nothing
safeIx (x:xs) n
  | n == 0 = pure x
  | n < 0 = Nothing
  | otherwise = safeIx xs (n - 1)

-- Adopted from Bodil Stokke's code so it's LGPL3
updateProps :: forall m el. (MonadIO m, IsDOMElement el) => DOMAPI -> el -> [(Text, Text)] -> [(Text, Text)] -> m ()
updateProps api me oldProps newProps =
  sequence_ (update <$> Map.keys (Map.union oldMap newMap))
  where
    oldMap = Map.fromList oldProps
    newMap = Map.fromList newProps
    update key =
      case (Map.lookup key oldMap, Map.lookup key newMap) of
        (Nothing, Just value) -> (setAttribute api) me key value
        (Just _, Nothing) -> (removeAttribute api) me key
        (Just prev, Just next) -> when (prev /= next) $ (setAttribute api) me key next
        (Nothing, Nothing) -> pure ()

-- https://github.com/bodil/purescript-vdom/blob/master/src/Data/VirtualDOM.purs
-- TODO: record stats about node addition/removal for benchmarking.
patch :: forall a m. (MonadIO m, IsDOMNode a) => DOMAPI -> a -> Maybe VNode -> Maybe VNode -> m ()
patch api target old new = patchIndexed target old new 0
  where
    patchIndexed :: forall a2 m2. (IsDOMNode a2, MonadIO m2) =>  a2 -> Maybe VNode -> Maybe VNode -> Int -> m2 ()
    patchIndexed _ Nothing Nothing _ = pure ()

    patchIndexed parent Nothing (Just new') _ = do
      el <- (createElement api) new'
      void $ (appendChild api) parent el

    patchIndexed parent (Just _) Nothing index = do
      child' <- childAt index parent
      case child' of
        Just child -> void $ dOMNodeRemoveChild parent child
        Nothing -> pure ()

    patchIndexed parent (Just (TextNode old')) (Just (TextNode new')) index =
      when (old' /= new') $ do
        me <- childAt index parent
        maybe (pure ()) (\me' -> (setTextContent api) me' new') me

    patchIndexed parent (Just old') (Just new') index = do
      me' <- childAt index parent
      case me' of
        Nothing -> pure ()
        Just me ->
          if (changed old' new') then do
            n <- (createElement api) new' -- TODO
            void $ (replaceChild api) parent n me
          else do
            -- TODO: unclear whether this cast can fail. Don't know
            -- enough about non-DOMElement DOMNodes.
            Just meEl <- liftIO (castTo DOMElement me)
            case (old', new') of
              (Element _ oldProps _ _ , Element _ newProps _ _) ->
                updateProps api meEl oldProps newProps
              (_, _) -> pure ()
            walkChildren me old' new'

    walkChildren :: forall a2 m2. (IsDOMNode a2, MonadIO m2) =>  a2 -> VNode -> VNode -> m2 ()
    walkChildren target' (Element _ _ _ oldChildren) (Element _ _ _ newChildren) = do
      let r = [0 .. ((max (length oldChildren) (length newChildren)) - 1)]
      sequence_ $ map (\i -> patchIndexed target' (safeIx oldChildren i) (safeIx newChildren i) i) r
    walkChildren _ _ _ = pure ()


newDOMAPI :: DOMDocument -> DOMAPI
newDOMAPI doc = DOMAPI
  { createElement = create
  , appendChild = dOMNodeAppendChild
  , replaceChild = dOMNodeReplaceChild
  , setTextContent = dOMNodeSetTextContent
  , setAttribute = dOMElementSetAttribute
  , removeAttribute = dOMElementRemoveAttribute
  }
  where
    create :: forall m. (MonadIO m) => VNode -> m DOMNode
    create (Element tagName' props' listeners' children') = do
      meEl <- dOMDocumentCreateElement doc tagName'
      Just me <- liftIO (castTo DOMNode meEl)
      -- listeners cannot be removed for now, the only way to remove a
      -- listener is to  remove the element.
      sequence_ $ map (\addHandler -> liftIO (addHandler meEl)) listeners'
      sequence_ $ map (\child -> do
                          new'' <- create child
                          dOMNodeAppendChild me new''
                      ) children'
      sequence_ $ map (\(attrName, value) -> dOMElementSetAttribute meEl attrName value) props'
      pure me
    create (TextNode t) = do
      node <- dOMDocumentCreateTextNode doc t
      Just me <- liftIO (castTo DOMNode node)
      pure me



--- Internal stuff

mkMouseHandler :: MouseHandler -> Closure
mkMouseHandler c = unsafePerformIO $
  wrapMouseEventClosureC (wrapMouseEventClosure c) >>= newCClosure

mkEventHandler :: EventHandler -> Closure
mkEventHandler c = unsafePerformIO $
  wrapEventClosureC (wrapEventClosure c) >>= newCClosure

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
