{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RankNTypes, NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings, OverloadedLabels, TypeApplications, ScopedTypeVariables #-}

module TestPlugin where

import qualified Prelude
import Protolude hiding (get, set, on)

import GI.GLib (idleAdd)
import Foreign.Ptr (Ptr, FunPtr)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr_)
import GI.WebKit2WebExtension
import Debug.Trace (traceEventIO)
import Gluon.VDom (VNode, newDOMAPI, DOMAPI, onClick, patch)
import Gluon.VDom.Elements (div_, p_, text_, style_)
import qualified Gluon.VDom.Elements as GE
import qualified Gluon.VDom.Attributes as GA

testVNode :: Int -> VNode
testVNode n =
  div_ [onClick testClosure2, GA.style_ "color: red"]
  [ GE.text_ "hello"
  , GE.button_ [GA.style_ $ "width: "<>(show (100 + n `mod` 100))<>"px"] [text_ (show n)]
  ]

event :: Prelude.String -> IO a -> IO a
event label =
  bracket_ (traceEventIO $ "START " ++ label)
           (traceEventIO $ "STOP "  ++ label)


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
      threadDelay (1000 * 10)
      idleAdd 0 rm -- <-- this is the magic function!

      where
        renderMore :: DOMAPI -> DOMHTMLElement -> MVar (Maybe VNode, Int) -> IO Bool
        renderMore api body state  = do
          (vdom, n) <- takeMVar state
          let newVDom = Just (testVNode n)
          putMVar state (newVDom, n + 1)
          event "P" $ patch api body vdom newVDom
          pure False


pageCreated_hs :: Ptr () -> Ptr WebPage -> Ptr () -> IO ()
pageCreated_hs = webExtensionPageCreatedCallbackWrapper pageCreated

foreign export ccall pageCreated_hs :: WebExtensionPageCreatedCallbackC

testClosure2 :: DOMMouseEvent -> IO ()
testClosure2 ev = do
  Just doc <- dOMEventGetSrcElement ev >>= castTo DOMElement
  x <- dOMMouseEventGetClientX ev
  y <- dOMMouseEventGetClientY ev
  pure ()
