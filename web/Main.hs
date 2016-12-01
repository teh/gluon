{-# LANGUAGE OverloadedStrings, OverloadedLabels, TypeApplications, ScopedTypeVariables #-}

import Protolude hiding (on, error, get)

import GI.Gtk hiding (main)
import qualified GI.Gtk as Gtk
import GI.WebKit2
import GI.Gio (memoryInputStreamNewFromData)

import System.Environment (getProgName)


main :: IO ()
main = do
  progName <- getProgName
  args <- getArgs

  _ <- Gtk.init (Just ((toS progName) : (map toS args)))

  win <- new Window
    [ #type := WindowTypeToplevel
    , #iconName := "applications-haskell"
    ]

  void $ on win #destroy mainQuit
  context <- webContextGetDefault
  void $ on context #initializeWebExtensions $ do
    webContextSetWebExtensionsDirectory context "./ext"

  -- register haskell scheme
  webContextRegisterUriScheme context "haskell" $ \request -> do
    path <- uRISchemeRequestGetUri request
    ret <- memoryInputStreamNewFromData "" Nothing
    print path
    uRISchemeRequestFinish request ret 14 (Just "text/html")

  view <- new WebView []
  #add win view
  #loadUri view "haskell://test"

  #showAll win

  Gtk.main
