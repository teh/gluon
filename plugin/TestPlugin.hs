{-# LANGUAGE ForeignFunctionInterface, NamedFieldPuns #-}
{-# LANGUAGE RankNTypes, NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings, OverloadedLabels, TypeApplications, ScopedTypeVariables #-}

module TestPlugin where

import qualified Prelude
import Protolude hiding (get, on)

import GI.GLib (idleAdd)
import Foreign.Ptr (Ptr)
import GI.WebKit2WebExtension
import Gluon.VDom (VNode, newDOMAPI, DOMAPI, patch)
import qualified Gluon.VDom.Elements as GE
import qualified Gluon.VDom.Attributes as GA
import qualified Gluon.VDom.Events as GE


data Todo = Todo
  { what :: Text
  , done :: Bool
  } deriving (Show, Eq)

data TodoState = TodoState
  { todos :: [Todo]
  , vdom :: Maybe VNode
  }


renderTodo :: Todo -> VNode
renderTodo Todo{what, done} =
  GE.ol_ [GA.class_ "list-group-item", GA.style_ (if done then "text-decoration: strike-through" else "")] [GE.text_ what]

renderTodos :: (DOMMouseEvent -> IO ()) -> TodoState -> VNode
renderTodos addTodo TodoState{todos} =
  GE.div_ [GA.class_ "container-fluid"]
  [ GE.h1_ [] [GE.text_ "TODO example"]
  , GE.div_ []
    [ GE.input_ [GA.type_ "text"] []
    , GE.button_ [GA.class_ "btn btn-primary", GE.onclick_ addTodo] [GE.text_ "Add TODO"]
    ]
  , GE.ul_ [GA.class_ "list-group"] (map renderTodo todos)
  ]

pageCreated :: WebPage -> IO ()
pageCreated page = do
  webPageGetId page >>= print

  void $ on page #documentLoaded $ do
    get page #uri >>= print
    doc <- webPageGetDomDocument page
    body <- dOMDocumentGetBody doc

    let api = newDOMAPI doc
    state' <- newMVar (TodoState [] Nothing)

    -- There's some knot-tying going on here because handlers modify the MVar
    -- state. They should actually modify just the state but that's for a higher
    -- level library to fix.
    let addTodoHandler = addTodo api body state'
    let vdom = renderTodos addTodoHandler (TodoState [] Nothing)
    patch api body Nothing (Just vdom)
    modifyMVar_ state' (\_ -> pure (TodoState [] (Just vdom)))

      where
        addTodo :: DOMAPI -> DOMHTMLElement -> MVar TodoState -> DOMMouseEvent -> IO ()
        addTodo api body state' _ = do
          s <- takeMVar state'
          let s' = s { todos = (Todo "hello" False):(todos s) }
          let newVDom = Just (renderTodos (addTodo api body state') s')
          putMVar state' (s' { vdom = newVDom })
          void $ patch api body (vdom s) newVDom

pageCreated_hs :: Ptr () -> Ptr WebPage -> Ptr () -> IO ()
pageCreated_hs = webExtensionPageCreatedCallbackWrapper pageCreated

foreign export ccall pageCreated_hs :: WebExtensionPageCreatedCallbackC
