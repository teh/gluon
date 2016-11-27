{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings, OverloadedLabels, TypeApplications, ScopedTypeVariables #-}

module TestPlugin where

import Protolude (Text, toS, (<>))

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr_)
import GI.WebKit2WebExtension

pageCreated :: WebPage -> IO ()
pageCreated page = do
  webPageGetId page >>= print
  on page #documentLoaded $ do
    get page #uri >>= print
    dom <- webPageGetDomDocument page
    el <- get dom #documentElement
    set el [#innerHtml := "<h2>Hello</h2>"]
    get el #innerHtml >>= print
    close <- wrapEventClosureC (wrapEventClosure testClosure) >>= newCClosure
    close2 <- wrapMouseEventClosureC (wrapMouseEventClosure (testClosure2 el)) >>= newCClosure
    _ <- dOMEventTargetAddEventListener el "click" close True
    _ <- dOMEventTargetAddEventListener el "mousemove" close2 True
    pure ()
  pure ()

pageCreated_hs :: Ptr () -> Ptr WebPage -> Ptr () -> IO ()
pageCreated_hs = webExtensionPageCreatedCallbackWrapper pageCreated

foreign export ccall pageCreated_hs :: WebExtensionPageCreatedCallbackC

testClosure :: DOMEvent -> IO ()
testClosure _ = do
  print "testClosure"

testClosure2 :: DOMElement -> DOMMouseEvent -> IO ()
testClosure2 doc ev = do
  x <- dOMMouseEventGetClientX ev
  y <- dOMMouseEventGetClientY ev
  set doc [#innerHtml := ("<h2>Mouse</h2>x: " <> toS (show x) <> ", y: " <> toS (show y))]
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
