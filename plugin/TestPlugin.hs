{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RankNTypes, NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings, OverloadedLabels, TypeApplications, ScopedTypeVariables #-}

module TestPlugin where

import qualified Prelude
import Protolude hiding (get, on)

import GI.GLib (idleAdd)
import Foreign.Ptr (Ptr)
import GI.WebKit2WebExtension
import Gluon.VDom (VNode, newDOMAPI, DOMAPI, patch, onClick)
import qualified Gluon.VDom.Elements as GE
import qualified Gluon.VDom.Attributes as GA

testVNode :: Int -> VNode
testVNode n =
  GE.div_ [onClick printCoordinates, GA.class_ "container"]
  [ GE.h1_ [] [GE.text_ "Hello"]
  , GE.div_ []
    [ GE.button_ [GA.class_ "btn btn-primary"]
      [GE.text_ (show n)]
    ]
  ]

pageCreated :: WebPage -> IO ()
pageCreated page = do
  webPageGetId page >>= print

  void $ on page #documentLoaded $ do
    get page #uri >>= print
    doc <- webPageGetDomDocument page
    body <- dOMDocumentGetBody doc
    docEl <- dOMDocumentGetDocumentElement doc
    state' <- newMVar (Nothing, 2)

    let rm = renderMore (newDOMAPI doc) body state'

    -- Somewhat crazily the document only renders correctly if we
    -- modify it in an event handler. I suspect this has to do with
    -- some "refresh" function we need to call but I haven't figured it out yet.
    void $ forkIO $ forever $ do
      idleAdd 0 rm -- <-- idleAdd is the magic function!
      threadDelay (1000 * 1000)

      where
        renderMore :: DOMAPI -> DOMHTMLElement -> MVar (Maybe VNode, Int) -> IO Bool
        renderMore api body state'  = do
          (vdom, n) <- takeMVar state'
          let newVDom = Just (testVNode n)
          putMVar state' (newVDom, n + 1)
          patch api body vdom newVDom
          pure False


pageCreated_hs :: Ptr () -> Ptr WebPage -> Ptr () -> IO ()
pageCreated_hs = webExtensionPageCreatedCallbackWrapper pageCreated

foreign export ccall pageCreated_hs :: WebExtensionPageCreatedCallbackC

printCoordinates :: DOMMouseEvent -> IO ()
printCoordinates ev = do
  Just doc <- dOMEventGetSrcElement ev >>= castTo DOMElement
  x <- dOMMouseEventGetClientX ev
  y <- dOMMouseEventGetClientY ev
  print (x, y)
  pure ()
