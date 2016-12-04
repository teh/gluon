module Gluon.VDom.Events
  ( EventHandler
  , MouseHandler
  , KeyboardHandler
  , onclick_
  , addHandler
  , HandlerDescription
  ) where

import Protolude
import qualified GHC.Show as GS
import GI.WebKit2WebExtension (DOMMouseEvent(..), DOMKeyboardEvent(..),
                               DOMEvent(..), dOMEventTargetAddEventListener,
                               DOMElement(..))
import GI.GLib (Closure, newCClosure)
import System.IO.Unsafe (unsafePerformIO)
import Foreign.Ptr (Ptr, FunPtr)
import Foreign.ForeignPtr (newForeignPtr_)


type EventHandler = DOMEvent -> IO ()
type MouseHandler = DOMMouseEvent -> IO ()
type KeyboardHandler = DOMKeyboardEvent -> IO ()

data HandlerDescription =
  MouseHandlerDescription Text (DOMMouseEvent -> IO ())
  | KeyboardHandlerDescription Text (DOMKeyboardEvent -> IO ())
  | GenericHandlerDescription Text (DOMEvent -> IO ())

-- Show instance breaks the show/read contract, only use for
-- debugging!
instance GS.Show HandlerDescription where
  show (MouseHandlerDescription event _) = "MouseHandler: " <> (toS event)
  show (KeyboardHandlerDescription event _) = "KeyboardHandler: " <> (toS event)
  show (GenericHandlerDescription event _) = "GenericHandler: " <> (toS event)

-- mouse handlers
onclick_ :: MouseHandler -> Either (Text, Text) HandlerDescription
onclick_ h = Right (MouseHandlerDescription "click" h)

onmousedown_ :: MouseHandler -> Either (Text, Text) HandlerDescription
onmousedown_ h = Right (MouseHandlerDescription "mousedown" h)

onmouseup_ :: MouseHandler -> Either (Text, Text) HandlerDescription
onmouseup_ h = Right (MouseHandlerDescription "mouseup" h)

onmousemove_ :: MouseHandler -> Either (Text, Text) HandlerDescription
onmousemove_ h = Right (MouseHandlerDescription "mousemove" h)



addHandler :: HandlerDescription -> DOMElement -> IO Bool
addHandler (MouseHandlerDescription event h) el = dOMEventTargetAddEventListener el event (mkMouseHandler h) True
addHandler (KeyboardHandlerDescription event h) el = dOMEventTargetAddEventListener el event (mkKeyboardHandler h) True
addHandler (GenericHandlerDescription event h) el = dOMEventTargetAddEventListener el event (mkEventHandler h) True

--- Internal stuff

mkMouseHandler :: MouseHandler -> Closure
mkMouseHandler c = unsafePerformIO $
  wrapMouseEventClosureC (wrapMouseEventClosure c) >>= newCClosure

mkKeyboardHandler :: KeyboardHandler -> Closure
mkKeyboardHandler c = unsafePerformIO $
  wrapKeyboardEventClosureC (wrapKeyboardEventClosure c) >>= newCClosure

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

wrapKeyboardEventClosure :: (DOMKeyboardEvent -> IO ()) -> EventHandlerCClosure DOMKeyboardEvent
wrapKeyboardEventClosure handler _ e = do
  e' <- newForeignPtr_ e
  let event = DOMKeyboardEvent e'
  handler event

foreign import ccall "wrapper" wrapEventClosureC :: EventHandlerCClosure DOMEvent -> IO (FunPtr (EventHandlerCClosure DOMEvent))
foreign import ccall "wrapper" wrapMouseEventClosureC :: EventHandlerCClosure DOMMouseEvent -> IO (FunPtr (EventHandlerCClosure DOMMouseEvent))
foreign import ccall "wrapper" wrapKeyboardEventClosureC :: EventHandlerCClosure DOMKeyboardEvent -> IO (FunPtr (EventHandlerCClosure DOMKeyboardEvent))
